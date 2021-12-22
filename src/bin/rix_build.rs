use rix::eval::Eval;
use rix::store::build::Worker;
use rix::store::LocalStore;
use rix::util::*;
use std::path::{Path, PathBuf};
use std::sync::Arc;

fn main() -> NixResult {
  logger::init()?;

  let eval = Eval::new(Arc::new(LocalStore::new()?));
  let mut realpath = PathBuf::from("/rix/store/xj18wv1765ns1wkb4fmmyx54lhm376vw-stdenv-linux.drv");
  if !realpath.exists() {
    let stdenv_drv = eval.eval_inline("with import <nixpkgs> {}; stdenv")?;

    let drvpath = &stdenv_drv
      .as_attrs()
      .ok_or_else(|| anyhow!("input is not a derivation"))?
      .get(&Ident::from("drvPath"))
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
