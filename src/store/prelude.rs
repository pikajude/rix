pub use super::{
  derivation::{Derivation, DerivationType, HashModulo, Output},
  path::{Hash as StorePathHash, Path as StorePath},
  Store,
};
pub use crate::util::*;
pub use anyhow::{Context as _, Result};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum FileIngestionMethod {
  Flat,
  Recursive,
}

impl FileIngestionMethod {
  pub fn prefix(&self) -> &'static str {
    match self {
      Self::Flat => "",
      Self::Recursive => "r:",
    }
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Repair {
  Off,
  On,
}
