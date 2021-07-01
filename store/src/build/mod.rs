use crate::prelude::*;
use crossbeam::thread::Scope;
use dep_queue::DependencyQueue;
use queue::Queue;
use std::{
  collections::{BTreeSet, HashMap, HashSet},
  sync::Arc,
  time::Duration,
};

mod dep_queue;
mod queue;

cfg_if::cfg_if! {
  if #[cfg(target_os = "linux")] {
    mod linux;
    use self::linux as sys;
  } else if #[cfg(target_os = "macos")] {
    mod macos;
    use self::macos as sys;
  } else {
    compile_error!("Nix builds are not supported on this platform.")
  }
}

#[derive(Debug, Deref, Copy, Clone)]
struct FinishedChild(u32);

#[derive(Debug)]
enum Message {
  Finish {
    job_id: usize,
    outputs: BTreeSet<String>,
    // If `Err`, the build failed and other builds should be killed. If `Ok`, optionally returns
    // the pid of a process that should be removed from self.active_pids.
    result: Result<Option<FinishedChild>>,
  },
  #[allow(dead_code)]
  SpawnedProcess(u32),
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Worker {
  queue: DependencyQueue<StorePath, String, Derivation>,
  pending: Vec<(StorePath, Derivation)>,
  active: HashMap<usize, StorePath>,
  messages: Arc<Queue<Message>>,
  next_id: usize,
  active_pids: HashSet<u32>,
  #[derivative(Debug = "ignore")]
  store: Arc<dyn Store>,
}

impl Worker {
  pub fn new(store: Arc<dyn Store>) -> Self {
    Self {
      store,
      queue: Default::default(),
      pending: Default::default(),
      active: Default::default(),
      messages: Arc::new(Queue::new(100)),
      next_id: Default::default(),
      active_pids: Default::default(),
    }
  }

  // FIXME: This method produces a dependency queue of X objects (i.e. the entire
  // dependency tree) even if everything has already been built.
  pub fn add_needed(&mut self, path: &StorePath) -> Result<()> {
    let drv = self.store.read_derivation(path)?;
    let deps_iter = drv
      .input_derivations
      .iter()
      .flat_map(|(path, outputs)| outputs.iter().map(move |o| (path.clone(), o.clone())))
      .collect::<Vec<(StorePath, String)>>();
    for path in drv.input_derivations.keys() {
      if !self.queue.dep_map.contains_key(path) {
        self.add_needed(path)?;
      }
    }
    self.queue.enqueue(path.clone(), drv, deps_iter);
    Ok(())
  }

  fn spawn_if_possible(&mut self, scope: &Scope) {
    while let Some((path, drv)) = self.queue.dequeue() {
      self.pending.push((path, drv));
    }

    while !self.pending.is_empty() && self.has_slots() {
      let (path, drv) = self.pending.remove(0);
      self.run(path, drv, scope);
    }
  }

  fn has_slots(&self) -> bool {
    self.active.len() < 2
  }

  fn wait_for_events(&mut self) -> Vec<Message> {
    let mut events = self.messages.try_pop_all();
    if events.is_empty() {
      loop {
        match self.messages.pop(Duration::from_millis(500)) {
          Some(message) => {
            events.push(message);
            break;
          }
          None => {
            trace!("waiting for events");
            continue;
          }
        }
      }
    }
    events
  }

  fn drain(&mut self, scope: &Scope) -> Result<()> {
    let mut error = None;

    loop {
      if error.is_none() {
        self.spawn_if_possible(scope);
      }

      if self.active.is_empty() {
        break;
      }

      for event in self.wait_for_events() {
        if let Err(e) = self.handle_event(event) {
          self.handle_error(&mut error, e);
        }
      }
    }

    if let Some(e) = error {
      Err(e)
    } else if self.queue.is_empty() && self.pending.is_empty() {
      Ok(())
    } else {
      bail!("internal error: some jobs left in queue")
    }
  }

  fn handle_error(&self, some_error: &mut Option<anyhow::Error>, error: anyhow::Error) {
    if some_error.is_some() {
      warn!("{:#}", error);
    } else {
      if !self.active.is_empty() {
        warn!("{:#}", error);
        eprintln!("build failed, waiting for others to finish");
      }
      *some_error = Some(error);
    }
  }

  fn handle_event(&mut self, event: Message) -> Result<()> {
    match event {
      Message::Finish {
        job_id,
        outputs,
        result,
      } => {
        let thingy = self.active.remove(&job_id).unwrap();
        for out in &outputs {
          self.queue.finish(&thingy, out);
        }
        let x = result?;
        debug!("build finished"; "path" => %thingy, "outputs" => ?outputs);
        if let Some(pid) = x {
          self.active_pids.remove(&*pid);
        }
      }
      Message::SpawnedProcess(pid) => {
        assert!(self.active_pids.insert(pid));
      }
    }
    Ok(())
  }

  fn run(&mut self, path: StorePath, drv: Derivation, scope: &Scope) {
    let id = self.next_id;
    self.next_id += 1;
    assert!(self.active.insert(id, path.clone()).is_none());

    debug!("starting build"; "path" => %path);

    let messages = Arc::clone(&self.messages);
    let store = Arc::clone(&self.store);

    let doit = move |scope: &Scope<'_>| {
      let mut result = Ok(None);

      let setup_failure: Result<()> = try {
        let mut needs_build = false;
        for (name, out) in drv.outputs.iter() {
          if !store.is_valid_path(&*out.path(&*store, &drv.name, name)?)? {
            needs_build = true;
            break;
          }
        }

        if !needs_build {
          messages.push(Message::Finish {
            job_id: id,
            outputs: drv.outputs.keys().cloned().collect(),
            result,
          });
        }
      };

      result = setup_failure.and_then(|_| sys::build(&*store, &messages, scope, &path, &drv));

      messages.push(Message::Finish {
        job_id: id,
        outputs: drv.outputs.keys().cloned().collect(),
        result,
      });
    };

    scope.spawn(doit);
  }

  pub fn build(mut self) -> Result<()> {
    self.queue.queue_finished();

    trace!("{:#?}", self.queue);

    crossbeam::thread::scope(|s| self.drain(s)).unwrap()
  }
}
