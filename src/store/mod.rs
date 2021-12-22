use crate::util::*;
use anyhow::Result;
pub use prelude::StorePath;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::io::Read;
use std::path::{Path, PathBuf};

pub mod build;
pub mod derivation;
mod local;
mod lock;
pub mod path;
pub mod path_info;
mod prelude;
pub mod refs;
pub mod settings;

pub use derivation::{Derivation, DerivationType, DrvName, HashModulo, Output};
pub use local::LocalStore;
pub use path_info::ValidPathInfo;
pub use prelude::{FileIngestionMethod, Repair};

pub type PathSet = BTreeSet<String>;
pub type StorePathSet = BTreeSet<StorePath>;

#[derive(Copy, Clone, Default)]
pub struct ClosureOpts {
  pub backwards: bool,
  pub include_outputs: bool,
  pub include_derivers: bool,
}

pub trait Store: Send + Sync {
  fn store_path(&self) -> &Path;

  fn parse_store_path(&self, path: &Path) -> Result<StorePath> {
    if path.parent() != Some(self.store_path()) {
      bail!(
        "path `{}' is not a direct descendant of the Nix store",
        path.display()
      );
    }

    StorePath::from_base_name(
      path
        .file_name()
        .and_then(|x| x.to_str())
        .ok_or_else(|| anyhow!("invalid filepath"))?,
    )
  }

  fn print_store_path(&self, path: &StorePath) -> String {
    self.to_real_path(path).display().to_string()
  }

  fn to_real_path(&self, path: &StorePath) -> PathBuf {
    self.store_path().join(path.to_string())
  }

  fn is_in_store(&self, path: &Path) -> bool {
    path.starts_with(self.store_path())
  }

  fn is_valid_path(&self, path: &StorePath) -> Result<bool> {
    self.query_path_info(path).map(|x| x.is_some())
  }

  fn compute_fs_closure(
    &self,
    path: &StorePath,
    closure: &mut StorePathSet,
    opts: ClosureOpts,
  ) -> Result<()>;

  /// Parse a derivation. The given path must be registered as valid in the
  /// database.
  fn read_derivation(&self, path: &StorePath) -> Result<Derivation>;

  /// Like `read_derivation`, but accepts a path that might not be in the Nix
  /// database yet.
  fn try_read_derivation(&self, path: &StorePath) -> Result<Derivation>;

  fn make_store_path(&self, path_type: &str, hash: Hash, name: &str) -> Result<StorePath> {
    let ident = format!(
      "{}:{}:{}:{}",
      path_type,
      hash.encode_with_type(Encoding::Base16),
      self.store_path().display(),
      name
    );
    let hash = Hash::new(&ident, HashType::SHA256).truncate(20);
    StorePath::from_parts(hash.as_bytes(), name)
  }

  fn make_output_path(&self, id: &str, hash: Hash, name: &str) -> Result<StorePath> {
    if id == "out" {
      self.make_store_path(&format!("output:{}", id), hash, name)
    } else {
      self.make_store_path(&format!("output:{}", id), hash, &format!("{}-{}", name, id))
    }
  }

  fn make_text_path(&self, name: &str, hash: Hash, refs: &StorePathSet) -> Result<StorePath> {
    ensure!(
      hash.ty() == HashType::SHA256,
      "make_text_path can only be used with SHA256"
    );
    self.make_store_path(&make_type(self, "text".into(), refs, false), hash, name)
  }

  fn make_fixed_output_path(
    &self,
    method: FileIngestionMethod,
    hash: Hash,
    name: &str,
    refs: &StorePathSet,
    self_referential: bool,
  ) -> Result<StorePath> {
    if hash.ty() == HashType::SHA256 && method == FileIngestionMethod::Recursive {
      self.make_store_path(
        &make_type(self, "source".into(), refs, self_referential),
        hash,
        name,
      )
    } else {
      ensure!(
        refs.is_empty(),
        "fixed-output paths with references may only be recursive SHA256"
      );
      self.make_store_path(
        "output:out",
        Hash::new(
          format!(
            "fixed:out:{prefix}:{hash}:",
            prefix = method.prefix(),
            hash = hash.encode_with_type(Encoding::Base16)
          ),
          HashType::SHA256,
        ),
        name,
      )
    }
  }

  fn write_derivation(
    &self,
    derivation: &Derivation,
    repair: Repair,
    read_only: bool,
  ) -> Result<StorePath> {
    let mut refs = derivation.input_sources.clone();
    refs.extend(derivation.input_derivations.keys().cloned());

    let suffix = format!("{}.drv", derivation.name);
    let contents = derivation.print(self, false, None).to_string();

    if read_only {
      self.store_path_for_text(&suffix, contents.as_ref(), &refs)
    } else {
      self.add_text_to_store(&suffix, contents.as_ref(), &refs, repair)
    }
  }

  fn hash_derivation_modulo(
    &self,
    derivation: &Derivation,
    mask_outputs: bool,
  ) -> Result<HashModulo> {
    let mut deferred = false;
    match derivation.ty {
      DerivationType::Fixed => {
        let mut output_hashes = HashMap::new();
        for (name, output) in derivation.outputs.iter() {
          let out_hash = output
            .as_fixed()
            .ok_or_else(|| anyhow!("fixed-output derivations must only have fixed outputs"))?;
          let hash = Hash::new(
            format!(
              "fixed:out:{method}:{hash}:{path}",
              method = out_hash.method_algo(),
              hash = out_hash.hash.encode(Encoding::Base16),
              path = self.print_store_path(&out_hash.store_path(self, &derivation.name, name)?)
            ),
            HashType::SHA256,
          );
          output_hashes.insert(name.clone(), hash);
        }
        return Ok(HashModulo::FixedOutput(output_hashes));
      }
      DerivationType::Floating => {
        deferred = true;
      }
      _ => {}
    }

    let mut inputs2 = BTreeMap::new();
    for (path, outputs) in derivation.input_derivations.iter() {
      match self.path_derivation_modulo(path)? {
        HashModulo::Known(h) => {
          inputs2.insert(h.encode(Encoding::Base16), outputs.clone());
        }
        HashModulo::Deferred(h) => {
          deferred = true;
          inputs2.insert(h.encode(Encoding::Base16), outputs.clone());
        }
        HashModulo::FixedOutput(hashes) => {
          for out in outputs {
            let h = hashes
              .get(out)
              .ok_or_else(|| anyhow!("hash missing for output `{}'", out))?;
            inputs2.insert(
              h.encode(Encoding::Base16),
              std::iter::once("out".into()).collect(),
            );
          }
        }
      }
    }

    let inner = Hash::new(
      derivation
        .print(self, mask_outputs, Some(inputs2))
        .to_string(),
      HashType::SHA256,
    );

    if deferred {
      Ok(HashModulo::Deferred(inner))
    } else {
      Ok(HashModulo::Known(inner))
    }
  }

  fn path_derivation_modulo(&self, drv_path: &StorePath) -> Result<HashModulo> {
    {
      if let Some(m) = derivation::DRV_HASHES.lock().get(drv_path).cloned() {
        return Ok(m);
      }
    }

    let drv = self.try_read_derivation(drv_path)?;
    let hash = self.hash_derivation_modulo(&drv, false)?;

    derivation::DRV_HASHES
      .lock()
      .insert(drv_path.clone(), hash.clone());

    Ok(hash)
  }

  fn add_path_to_store(
    &self,
    name: &str,
    path: &Path,
    method: FileIngestionMethod,
    hash_type: HashType,
    path_filter: &PathFilter,
    repair: Repair,
  ) -> Result<StorePath>;

  fn add_text_to_store(
    &self,
    name: &str,
    contents: &[u8],
    refs: &StorePathSet,
    repair: Repair,
  ) -> Result<StorePath>;

  fn add_dump_to_store(
    &self,
    source: Box<dyn Read>,
    name: &str,
    method: FileIngestionMethod,
    algo: HashType,
    repair: Repair,
  ) -> Result<StorePath>;

  fn add_to_store(
    &self,
    path_info: ValidPathInfo,
    source: Box<dyn Read>,
    repair: Repair,
  ) -> Result<()>;

  fn store_path_for_text(
    &self,
    name: &str,
    contents: &[u8],
    refs: &StorePathSet,
  ) -> Result<StorePath> {
    self.make_text_path(name, Hash::new(contents, HashType::SHA256), refs)
  }

  fn add_temp_root(&self, path: &StorePath) -> Result<()>;

  fn realise_context(&self, context: &PathSet) -> Result<()>;

  fn check_uri(&self, uri: &str) -> Result<()>;

  fn register_valid_paths(&self, infos: Vec<ValidPathInfo>) -> Result<()>;

  fn register_valid_path(&self, info: ValidPathInfo) -> Result<()> {
    self.register_valid_paths(vec![info])
  }

  fn query_path_info(&self, path: &StorePath) -> Result<Option<ValidPathInfo>>;

  fn get_path_info(&self, path: &StorePath) -> Result<ValidPathInfo> {
    self
      .query_path_info(path)?
      .ok_or_else(|| anyhow!("path {} is not valid", self.print_store_path(path)))
  }

  fn log_file_of(&self, path: &StorePath) -> PathBuf;
}

fn make_type<S: Store + ?Sized>(
  store: &S,
  mut ty: String,
  refs: &StorePathSet,
  self_referential: bool,
) -> String {
  for r in refs {
    ty.push(':');
    ty.push_str(&store.print_store_path(r));
  }
  if self_referential {
    ty.push_str(":self");
  }
  ty
}
