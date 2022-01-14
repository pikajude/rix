use super::{FileIngestionMethod, HashType, Store, StorePath};
pub use name::Name as DrvName;
use parking_lot::Mutex;
pub use print::Print;
use rix_util::*;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::PathBuf;

mod name;
mod parse;
mod print;

lazy_static! {
  pub(crate) static ref DRV_HASHES: Mutex<HashMap<StorePath, HashModulo>> = Default::default();
}

#[derive(Debug, Eq, PartialEq, Clone, EnumAsInner)]
pub enum HashModulo {
  Known(Hash),
  FixedOutput(HashMap<String, Hash>),
  Deferred(Hash),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
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

#[derive(Debug, Eq, PartialEq, Clone, EnumAsInner, Hash)]
pub enum Output {
  InputAddressed(StorePath),
  Fixed(FixedOutputHash),
  Floating(FileIngestionMethod, HashType),
  Deferred,
}

impl Output {
  pub fn get_path<S: Store + ?Sized>(
    &self,
    store: &S,
    drv_name: &str,
    output_name: &str,
  ) -> Result<Option<Cow<StorePath>>> {
    Ok(match self {
      Output::InputAddressed(i) => Some(Cow::Borrowed(i)),
      Output::Fixed(f) => Some(Cow::Owned(f.store_path(store, drv_name, output_name)?)),
      Output::Floating(_, _) => None,
      Output::Deferred => None,
    })
  }

  pub fn path<S: Store + ?Sized>(
    &self,
    store: &S,
    drv_name: &str,
    output_name: &str,
  ) -> Result<Cow<StorePath>> {
    self
      .get_path(store, drv_name, output_name)?
      .ok_or_else(|| anyhow!("output {:?} is required to have a specified path", self))
  }
}

pub fn output_path_name(drv_name: impl AsRef<str>, output_name: impl AsRef<str>) -> String {
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

impl Derivation {
  pub fn is_builtin(&self) -> bool {
    self.as_builtin().is_some()
  }

  pub fn as_builtin(&self) -> Option<String> {
    self
      .builder
      .to_string_lossy()
      .strip_prefix("builtin:")
      .map(|x| x.to_string())
  }

  pub fn is_fixed(&self) -> bool {
    matches!(self.ty, DerivationType::Fixed)
  }

  pub fn is_impure(&self) -> bool {
    matches!(self.ty, DerivationType::Fixed)
  }

  pub fn outputs_and_opt_paths<S: Store + ?Sized>(&self, store: &S) -> Result<OutputsAndPaths> {
    let mut o = OutputsAndPaths::new();
    for (name, out) in self.outputs.clone() {
      let pt = out
        .get_path(store, &self.name, &name)?
        .map(|x| x.into_owned());
      o.insert(name, (out, pt));
    }
    Ok(o)
  }
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
