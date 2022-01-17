use rix_core::build::Worker;
use rix_core::eval::Eval;
use rix_core::local_store::LocalStore;
use rix_util::*;
use std::path::Path;
use std::sync::Arc;

fn main() -> NixResult {
  init_rix()?;

  let eval = Eval::new(Arc::new(LocalStore::new()?));
  let stdenv_drv = eval.eval_inline("with import ~/.code/pkgs {}; stdenv.cc")?;

  let drvpath = &stdenv_drv
    .as_attrs()
    .ok_or_else(|| anyhow!("input is not a derivation"))?
    .get(&ident!("drvPath"))
    .ok_or_else(|| anyhow!("input is not a derivation"))?
    .v;

  let realpath = Path::new(&eval.force_string(Pos::none(), drvpath)?.s).to_path_buf();

  let drvpath = eval.store.parse_store_path(&realpath)?;

  let mut worker = Worker::new(eval.store.clone());
  worker.add_needed_all(&drvpath)?;
  worker.build()?;

  ok()
}
