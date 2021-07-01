use std::{
  collections::VecDeque,
  sync::{Condvar, Mutex},
  time::Duration,
};

#[derive(Debug)]
pub struct Queue<T> {
  state: Mutex<State<T>>,
  popper_cv: Condvar,
  bounded_cv: Condvar,
  bound: usize,
}

#[derive(Debug)]
struct State<T> {
  items: VecDeque<T>,
}

impl<T> Queue<T> {
  pub fn new(bound: usize) -> Queue<T> {
    Queue {
      state: Mutex::new(State {
        items: VecDeque::new(),
      }),
      popper_cv: Condvar::new(),
      bounded_cv: Condvar::new(),
      bound,
    }
  }

  pub fn push(&self, item: T) {
    self.state.lock().unwrap().items.push_back(item);
    self.popper_cv.notify_one();
  }

  pub fn _push_bounded(&self, item: T) {
    let locked_state = self.state.lock().unwrap();
    let mut state = self
      .bounded_cv
      .wait_while(locked_state, |s| s.items.len() >= self.bound)
      .unwrap();
    state.items.push_back(item);
    self.popper_cv.notify_one();
  }

  pub fn pop(&self, timeout: Duration) -> Option<T> {
    let (mut state, result) = self
      .popper_cv
      .wait_timeout_while(self.state.lock().unwrap(), timeout, |s| s.items.is_empty())
      .unwrap();
    if result.timed_out() {
      None
    } else {
      let value = state.items.pop_front()?;
      if state.items.len() < self.bound {
        // Assumes threads cannot be canceled.
        self.bounded_cv.notify_one();
      }
      Some(value)
    }
  }

  pub fn try_pop_all(&self) -> Vec<T> {
    let mut state = self.state.lock().unwrap();
    let result = state.items.drain(..).collect();
    self.bounded_cv.notify_all();
    result
  }
}
