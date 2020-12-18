use crate::{prelude::*, FileIngestionMethod, HashType};
use parking_lot::Mutex;
pub use print::Print;
use std::{
  collections::{BTreeMap, BTreeSet, HashMap},
  path::PathBuf,
};

mod parse;
mod print;

lazy_static! {
  pub(crate) static ref DRV_HASHES: Mutex<HashMap<StorePath, HashModulo>> = Default::default();
}

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

  pub fn store_path<S: Store + ?Sized>(
    &self,
    store: &S,
    drv_name: &str,
    output_name: &str,
  ) -> Result<StorePath> {
    store.make_fixed_output_path(
      self.method,
      self.hash,
      &output_path_name(drv_name, output_name),
      &Default::default(),
      false,
    )
  }
}

#[derive(Debug, Eq, PartialEq, Clone, EnumAsInner)]
pub enum Output {
  InputAddressed(StorePath),
  Fixed(FixedOutputHash),
  Floating(FileIngestionMethod, HashType),
  Deferred,
}

fn output_path_name(drv_name: impl AsRef<str>, output_name: impl AsRef<str>) -> String {
  let drv_name = drv_name.as_ref();
  let output_name = output_name.as_ref();
  if output_name == "out" {
    drv_name.to_string()
  } else {
    format!("{}-{}", drv_name, output_name)
  }
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
