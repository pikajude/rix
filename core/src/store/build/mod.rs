use crate::store::prelude::*;
use crossbeam::thread::Scope;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Duration;

use self::dep_queue::DependencyQueue;
use self::queue::Queue;

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
    compile_error!("Nix builds are not supported on this platform.");
  }
}

#[derive(Debug)]
enum Message {
  Finish(usize, Vec<String>, Result<()>),
}

type DerivationKey = StorePath;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Worker {
  queue: DependencyQueue<(DerivationKey, String), (), Derivation>,
  active: HashMap<usize, DerivationKey>,
  next_id: usize,
  messages: Arc<Queue<Message>>,
  #[derivative(Debug = "ignore")]
  store: Arc<dyn Store>,
}

impl Worker {
  pub fn new(store: Arc<dyn Store>) -> Self {
    Self {
      store,
      queue: Default::default(),
      active: Default::default(),
      messages: Arc::new(Queue::new(10)),
      next_id: 1,
    }
  }

  pub fn add_needed_all(&mut self, path: &DerivationKey) -> Result<()> {
    let drv = self.store.read_derivation(path)?;
    for out in drv.outputs.keys() {
      self.add_needed((path.clone(), out.clone()))?;
    }
    Ok(())
  }

  pub fn add_needed(&mut self, path: (DerivationKey, String)) -> Result<()> {
    if self.queue.dep_map.contains_key(&path) {
      return Ok(());
    }
    let drv = self.store.read_derivation(&path.0)?;
    let mut deps = vec![];
    for (drv_path, drv_outs) in &drv.input_derivations {
      let input_drv = self.store.read_derivation(drv_path)?;
      for drv_out in drv_outs {
        let dep_out_path =
          input_drv.outputs[drv_out].get_path(&*self.store, &input_drv.name, drv_out)?;
        if let Some(p) = dep_out_path {
          if self.store.is_valid_path(&*p)? {
            continue;
          }
        }
        deps.push(((drv_path.clone(), drv_out.clone()), ()));
      }
    }

    for missing in &deps {
      self.add_needed(missing.0.clone())?;
    }

    let cost = if drv.is_builtin() { 1 } else { 10 };
    self.queue.queue(path, drv, deps, cost);

    Ok(())
  }

  fn has_slots(&self) -> bool {
    self.active.is_empty()
  }

  fn try_spawn(&mut self, scope: &Scope) {
    loop {
      if !self.has_slots() {
        break;
      }
      match self.queue.dequeue() {
        None => break,
        Some((path, drv)) => self.spawn(path.0, drv, scope),
      }
    }
  }

  fn spawn(&mut self, path: StorePath, drv: Derivation, scope: &Scope) {
    let id = self.next_id;
    self.next_id += 1;
    assert!(self.active.insert(id, path.clone()).is_none());

    let store = Arc::clone(&self.store);
    let messages = Arc::clone(&self.messages);

    scope.spawn(move |scope| {
      let res = sys::build(&*store, scope, &path, &drv);
      messages.push(Message::Finish(
        id,
        drv.outputs.keys().cloned().collect(),
        res,
      ));
    });
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
    let mut err = None;

    loop {
      if err.is_none() {
        self.try_spawn(scope);
      }

      if self.active.is_empty() {
        break;
      }

      for event in self.wait_for_events() {
        if let Err(e) = self.handle_event(event) {
          self.handle_error(&mut err, e);
        }
      }
    }

    if let Some(e) = err {
      Err(e)
    } else if self.queue.is_empty() && self.active.is_empty() {
      Ok(())
    } else {
      bail!("internal error: jobs left in queue")
    }
  }

  fn handle_event(&mut self, event: Message) -> Result<()> {
    match event {
      Message::Finish(id, outputs, result) => {
        let drv_path = self.active.remove(&id).expect("incorrect ID");
        for out in &outputs {
          self.queue.finish(&(drv_path.clone(), out.clone()), &());
        }
        let _ = result?;
        debug!("build finished"; "path" => %drv_path, "outputs" => ?outputs);
        Ok(())
      }
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

  pub fn build(mut self) -> Result<()> {
    self.queue.queue_finished();

    crossbeam::thread::scope(|scope| self.drain(scope)).unwrap()
  }
}
