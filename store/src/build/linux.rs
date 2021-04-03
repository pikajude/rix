use super::*;
use crossbeam::thread::Scope;

const SANDBOX_UID: libc::uid_t = 1000;
const SANDBOX_GID: libc::uid_t = 100;

pub(super) fn build<S: Store + ?Sized>(
  store: &S,
  messages: &Queue<Message>,
  scope: &Scope<'_>,
  path: &StorePath,
  drv: &Derivation,
) -> Result<Option<FinishedChild>> {
  let build_log_path = store.log_file_of(path);

  std::fs::create_dir_all(build_log_path.parent().unwrap())?;

  let mut input_paths = BTreeSet::new();
  for (input_path, outputs) in drv.input_derivations.iter() {
    let input_drv = store.read_derivation(input_path)?;
    for out_name in outputs {
      if let Some(out) = input_drv.outputs.get(out_name) {
        store.compute_fs_closure(&*out.path(store, &drv.name, out_name)?, &mut input_paths)?;
      } else {
        bail!(
          "derivation {} requires nonexistent output {} from derivation {}",
          path,
          out_name,
          input_path
        );
      }
    }
  }

  for src in drv.input_sources.iter() {
    store.compute_fs_closure(src, &mut input_paths)?;
  }

  debug!("added input paths"; "paths" => ?input_paths);

  let builder_tmp = tempfile::Builder::new()
    .prefix(format!("nix-build-{}-", drv.name).as_str())
    .tempdir()?;

  debug!("running in tempdir: {}", builder_tmp.path().display());

  bail!("todo")
}
