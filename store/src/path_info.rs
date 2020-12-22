use std::time::SystemTime;

use crate::{prelude::*, StorePathSet};

#[derive(Derivative)]
#[derivative(Debug)]
pub struct ValidPathInfo {
  pub path: StorePath,
  pub deriver: Option<StorePath>,
  pub nar_hash: Hash,
  pub nar_size: Option<usize>,
  pub refs: StorePathSet,
  pub registration_time: Option<SystemTime>,
  // for sqlite
  #[derivative(Debug = "ignore")]
  id: i64,
}

impl ValidPathInfo {
  pub fn ultimate(&self) -> bool {
    todo!()
  }

  pub fn new(path: StorePath, nar_hash: Hash) -> Self {
    Self {
      path,
      nar_hash,
      deriver: None,
      nar_size: None,
      refs: Default::default(),
      registration_time: None,
      id: 0,
    }
  }
}

impl PartialEq for ValidPathInfo {
  fn eq(&self, other: &Self) -> bool {
    self.path == other.path && self.nar_hash == other.nar_hash && self.refs == other.refs
  }
}

impl Eq for ValidPathInfo {}

impl std::hash::Hash for ValidPathInfo {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.nar_hash.hash(state)
  }
}
