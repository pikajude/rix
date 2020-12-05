#![feature(pattern)]

#[macro_use] extern crate anyhow;
#[macro_use] extern crate derive_more;
#[macro_use] extern crate enum_as_inner;
#[macro_use] extern crate lazy_static;

use anyhow::Result;
use derivation::{DerivationType, HashModulo};
use hash::Encoding;
use prelude::StorePath;
use std::{
  collections::{BTreeMap, BTreeSet, HashMap},
  path::{Path, PathBuf},
};

pub mod base32;
pub mod derivation;
pub mod hash;
pub mod path;
mod prelude;

pub use derivation::Derivation;
pub use hash::{Hash, HashType};
pub use prelude::{FileIngestionMethod, Repair};

pub type StorePathSet = BTreeSet<StorePath>;

pub trait Store: Send + Sync {
  fn store_path(&self) -> &Path;

  fn parse_store_path<P: AsRef<Path>>(&self, path: P) -> Result<StorePath> {
    let path = path.as_ref();
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

  fn is_in_store<P: AsRef<Path>>(&self, path: P) -> bool {
    path.as_ref().starts_with(self.store_path())
  }

  fn compute_fs_closure(&self, path: &StorePath, closure: &mut StorePathSet) -> Result<()>;

  fn read_derivation(&self, path: &StorePath) -> Result<Derivation>;

  fn make_store_path<T: AsRef<str>, N: AsRef<str>>(
    &self,
    path_type: T,
    hash: Hash,
    name: N,
  ) -> Result<StorePath> {
    let name = name.as_ref();
    let ident = format!(
      "{}:{}:{}:{}",
      path_type.as_ref(),
      hash.encode_with_type(Encoding::Base16),
      self.store_path().display(),
      name
    );
    let hash = Hash::hash(&ident, HashType::SHA256)
      .truncate(20)
      .into_owned();
    StorePath::from_parts(hash.as_bytes(), name)
  }

  fn make_output_path<I: AsRef<str>, N: AsRef<str>>(
    &self,
    id: I,
    hash: Hash,
    name: N,
  ) -> Result<StorePath> {
    let id = id.as_ref();
    if id == "out" {
      self.make_store_path(format!("output:{}", id), hash, name)
    } else {
      self.make_store_path(
        format!("output:{}", id),
        hash,
        format!("{}-{}", name.as_ref(), id),
      )
    }
  }

  fn make_text_path<N: AsRef<str>>(
    &self,
    name: N,
    hash: Hash,
    refs: &StorePathSet,
  ) -> Result<StorePath> {
    ensure!(
      hash.ty() == HashType::SHA256,
      "make_text_path can only be used with SHA256"
    );
    self.make_store_path(make_type(self, "text".into(), refs, false), hash, name)
  }

  fn make_fixed_output_path<S: AsRef<str>>(
    &self,
    method: FileIngestionMethod,
    hash: Hash,
    name: S,
    refs: &StorePathSet,
    self_referential: bool,
  ) -> Result<StorePath> {
    if hash.ty() == HashType::SHA256 && method == FileIngestionMethod::Recursive {
      self.make_store_path(
        make_type(self, "source".into(), refs, self_referential),
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
        Hash::hash(
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
    let contents = derivation.unparse(self, false, None).to_string();

    if read_only {
      self.store_path_for_text(suffix, contents, &refs)
    } else {
      self.add_text_to_store(suffix, contents, &refs, repair)
    }
  }

  fn hash_derivation_modulo(
    &self,
    derivation: &Derivation,
    mask_outputs: bool,
  ) -> Result<HashModulo> {
    match derivation.ty {
      DerivationType::Fixed => {
        let mut output_hashes = HashMap::new();
        for (name, output) in derivation.outputs.iter() {
          let (out_hash, out_path) = output
            .as_fixed()
            .ok_or_else(|| anyhow!("fixed-output derivations must only have fixed outputs"))?;
          let hash = Hash::hash(
            format!(
              "fixed:out:{method}:{hash}:{path}",
              method = out_hash.method_algo(),
              hash = out_hash.hash.encode(Encoding::Base16),
              path = self.print_store_path(out_path)
            ),
            HashType::SHA256,
          );
          output_hashes.insert(name.clone(), hash);
        }
        return Ok(HashModulo::FixedOutput(output_hashes));
      }
      DerivationType::Floating => return Ok(HashModulo::Unknown),
      _ => {}
    }

    let mut inputs2 = BTreeMap::new();
    for (path, outputs) in derivation.input_derivations.iter() {
      match self.path_derivation_modulo(path)? {
        HashModulo::Normal(h) => {
          inputs2.insert(h.encode(Encoding::Base16), outputs.clone());
        }
        HashModulo::Unknown => {
          return Ok(HashModulo::Unknown);
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

    Ok(HashModulo::Normal(Hash::hash(
      derivation
        .unparse(self, mask_outputs, Some(inputs2))
        .to_string(),
      HashType::SHA256,
    )))
  }

  fn path_derivation_modulo(&self, drv_path: &StorePath) -> Result<HashModulo>;

  fn add_text_to_store<N: AsRef<str>, C: AsRef<str>>(
    &self,
    name: N,
    contents: C,
    refs: &StorePathSet,
    repair: Repair,
  ) -> Result<StorePath>;

  fn store_path_for_text<N: AsRef<str>, C: AsRef<[u8]>>(
    &self,
    name: N,
    contents: C,
    refs: &StorePathSet,
  ) -> Result<StorePath> {
    self.make_text_path(name, Hash::hash(contents, HashType::SHA256), refs)
  }
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
