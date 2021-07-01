use crate::store::{
  lock::UserLocker,
  settings::{settings, SandboxMode, Settings},
};

use super::*;
use crossbeam::thread::Scope;
use nix::unistd::{chown, Gid, Uid};
use tempfile::tempdir;

const SANDBOX_UID: libc::uid_t = 1000;
const SANDBOX_GID: libc::uid_t = 100;

pub(super) fn build<S: Store + ?Sized>(
  store: &S,
  messages: &Queue<Message>,
  scope: &Scope<'_>,
  path: &StorePath,
  drv: &Derivation,
) -> Result<Option<FinishedChild>> {
  let build_user = UserLocker::get()
    .find()?
    .ok_or_else(|| anyhow!("no UIDs available for build"))?;

  let build_log_path = store.log_file_of(path);

  std::fs::create_dir_all(build_log_path.parent().unwrap())?;

  let no_chroot = drv.env.get("__noChroot").map_or(false, |x| x == "1");
  let use_chroot;
  if settings().sandbox_mode() == SandboxMode::On {
    if no_chroot {
      bail!(
        "derivation '{}' has __noChroot set, which is not allowed",
        store.print_store_path(path)
      );
    }
    use_chroot = true;
  } else if settings().sandbox_mode() == SandboxMode::Off {
    use_chroot = false;
  } else {
    use_chroot = !no_chroot;
  }

  let build_tmp_dir = tempfile::Builder::new()
    .prefix(&format!("nix-build-{}-", &drv.name))
    .tempdir()?;

  chown(
    build_tmp_dir.path(),
    Some(Uid::from_raw(build_user.user().uid())),
    Some(Gid::from_raw(build_user.user().primary_group_id())),
  )
  .with_context(|| {
    format!(
      "unable to chown builder temp directory {}",
      build_tmp_dir.path().display()
    )
  })?;

  Ok(None)
}
