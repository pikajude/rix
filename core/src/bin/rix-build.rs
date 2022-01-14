use rix_core::build::Worker;
use rix_core::eval::Eval;
use rix_core::local_store::LocalStore;
use rix_util::*;
use std::path::{Path, PathBuf};
use std::sync::Arc;

fn main() -> NixResult {
  logger::init()?;

  let eval = Eval::new(Arc::new(LocalStore::new()?));
  let mut realpath = PathBuf::from("/rix/store/xj18wv1765ns1wkb4fmmyx54lhm376vw-stdenv-linux.drv");
  if !realpath.exists() {
    let stdenv_drv = eval.eval_inline("with import <nixpkgs> {}; stdenv.cc")?;

    let drvpath = &stdenv_drv
      .as_attrs()
      .ok_or_else(|| anyhow!("input is not a derivation"))?
      .get(&ident!("drvPath"))
      .ok_or_else(|| anyhow!("input is not a derivation"))?
      .v;

    realpath = Path::new(&eval.force_string(Pos::none(), drvpath)?.s).to_path_buf();
  }

  let drvpath = eval.store.parse_store_path(&realpath)?;

  let mut worker = Worker::new(eval.store.clone());
  worker.add_needed_all(&drvpath)?;
  worker.build()?;

  ok()
}
