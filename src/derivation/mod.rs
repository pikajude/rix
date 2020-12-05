use crate::{prelude::*, FileIngestionMethod, HashType};
pub use parse::Print;
use std::{
  collections::{BTreeMap, BTreeSet, HashMap},
  path::PathBuf,
};

mod parse;

#[derive(Debug, Eq, PartialEq, Clone, EnumAsInner)]
pub enum HashModulo {
  Normal(Hash),
  FixedOutput(HashMap<String, Hash>),
  Unknown,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct FixedOutputHash {
  pub method: FileIngestionMethod,
  pub hash: Hash,
}

impl FixedOutputHash {
  pub fn method_algo(&self) -> String {
    format!("{}{}", self.method.prefix(), self.hash.ty())
  }
}

#[derive(Debug, Eq, PartialEq, Clone, EnumAsInner)]
pub enum Output {
  InputAddressed(StorePath),
  Fixed(FixedOutputHash, StorePath),
  Floating(FileIngestionMethod, HashType),
  Deferred,
}

#[derive(Default, Debug, Clone)]
pub struct Derivation {
  pub name: String,
  pub builder: PathBuf,
  pub platform: String,
  pub args: Vec<String>,
  pub ty: DerivationType,
  pub env: BTreeMap<String, String>,
  pub input_sources: BTreeSet<StorePath>,
  pub outputs: BTreeMap<String, Output>,
  pub input_derivations: BTreeMap<StorePath, BTreeSet<String>>,
}

pub type OutputsAndPaths = BTreeMap<String, (Output, Option<StorePath>)>;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub enum DerivationType {
  InputAddressed,
  DeferredInputAddressed,
  Fixed,
  Floating,
}

impl DerivationType {
  pub fn is_impure(self) -> bool {
    matches!(self, Self::Fixed)
  }

  pub fn is_fixed(self) -> bool {
    matches!(self, Self::Fixed)
  }

  pub fn is_content_addressed(self) -> bool {
    matches!(self, Self::Fixed | Self::Floating)
  }
}

impl Default for DerivationType {
  fn default() -> Self {
    Self::InputAddressed
  }
}
