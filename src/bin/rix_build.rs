use rix::{
  eval::Eval,
  store::{build::Worker, LocalStore},
  util::*,
};
use std::{path::Path, sync::Arc};

fn main() -> NixResult {
  logger::init()?;

  let eval = Eval::new(Arc::new(LocalStore::new()?));
  let stdenv_drv = eval.eval_inline("with import <nixpkgs> {}; stdenv")?;

  let drvpath = &stdenv_drv
    .as_attrs()
    .ok_or_else(|| anyhow!("input is not a derivation"))?
    .get(&Ident::from("drvPath"))
    .ok_or_else(|| anyhow!("input is not a derivation"))?
    .v;

  let drvpath = eval.force_string(Pos::none(), &drvpath)?;
  let drvpath = eval.store.parse_store_path(Path::new(&*drvpath.s))?;

  let mut worker = Worker::new(eval.store.clone());
  worker.add_needed(&drvpath)?;
  worker.build()?;

  ok()
}
