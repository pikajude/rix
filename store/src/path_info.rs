use super::{StorePath, StorePathSet};
use rix_util::*;
use std::time::SystemTime;

#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub struct ValidPathInfo {
  pub path: StorePath,
  pub deriver: Option<StorePath>,
  pub nar_hash: Hash,
  pub nar_size: Option<usize>,
  pub refs: StorePathSet,
  pub registration_time: Option<SystemTime>,
  pub ultimate: bool,
  // for sqlite
  #[doc(hidden)]
  #[derivative(Debug = "ignore")]
  pub id: i64,
}

impl ValidPathInfo {
  pub fn new(path: StorePath, nar_hash: Hash) -> Self {
    Self {
      path,
      nar_hash,
      deriver: None,
      ultimate: false,
      nar_size: None,
      refs: Default::default(),
      registration_time: None,
      id: 0,
    }
  }

  pub fn registration_time_sql(&self) -> i64 {
    self.registration_time.map_or(0, |t| {
      t.duration_since(SystemTime::UNIX_EPOCH)
        .expect("registration type is earlier than 0")
        .as_secs() as _
    })
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
