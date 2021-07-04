use crate::store::{
  derivation::output_path_name,
  lock::UserLocker,
  settings::{settings, SandboxMode, Settings},
};

use super::*;
use crossbeam::thread::Scope;
use nix::unistd::{chown, Gid, Uid};

const SANDBOX_UID: libc::uid_t = 1000;
const SANDBOX_GID: libc::uid_t = 100;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum PathStatus {
  Corrupt,
  Absent,
  Valid,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct InitialOutputStatus {
  path: StorePath,
  status: PathStatus,
}

impl InitialOutputStatus {
  fn is_present(&self) -> bool {
    matches!(self.status, PathStatus::Corrupt | PathStatus::Valid)
  }

  fn is_valid(&self) -> bool {
    self.status == PathStatus::Valid
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InitialOutput {
  wanted: bool,
  output_hash: Hash,
  known: Option<InitialOutputStatus>,
}

fn static_output_hashes<'d, S: Store + ?Sized>(
  store: &S,
  drv: &'d Derivation,
) -> Result<Box<dyn Iterator<Item = (String, Hash)> + 'd>> {
  match store.hash_derivation_modulo(drv, true)? {
    HashModulo::Known(h) | HashModulo::Deferred(h) => Ok(Box::new(
      drv.outputs.keys().map(move |x| (x.to_string(), h)),
    )),
    HashModulo::FixedOutput(e) => Ok(Box::new(e.into_iter())),
  }
}

fn make_fallback_from_output<S: Store + ?Sized>(
  store: &S,
  path: &StorePath,
  drv: &Derivation,
  output_name: &str,
) -> Result<StorePath> {
  store.make_store_path(
    &format!("rewrite:{}:name:{}", path.to_string(), output_name),
    Hash::new_allow_empty("", Some(HashType::SHA256))?,
    &output_path_name(&drv.name, output_name),
  )
}

fn make_fallback_from_path<S: Store + ?Sized>(
  store: &S,
  drv_path: &StorePath,
  path: &StorePath,
) -> Result<StorePath> {
  store.make_store_path(
    &format!("rewrite:{}:{}", drv_path.to_string(), path.to_string()),
    Hash::new_allow_empty("", Some(HashType::SHA256))?,
    path.name(),
  )
}

pub(super) fn build<S: Store + ?Sized>(
  store: &S,
  messages: &Queue<Message>,
  scope: &Scope<'_>,
  path: &StorePath,
  drv: &Derivation,
) -> Result<Option<FinishedChild>> {
  let initial_outputs = static_output_hashes(store, drv)?
    .map(|(name, hash)| {
      (
        name,
        InitialOutput {
          wanted: true,
          output_hash: hash,
          known: None,
        },
      )
    })
    .collect::<HashMap<_, _>>();

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

  /*
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
  */

  let mut scratch_outputs = HashMap::new();
  for (output_name, status) in initial_outputs {
    let scratch_path = match status.known {
      None => make_fallback_from_output(store, path, drv, &output_name)?,
      Some(k) => {
        if use_chroot {
          k.path
        } else if !k.is_present() {
          k.path
        } else if !k.is_valid() {
          k.path
        } else {
          make_fallback_from_path(store, path, &k.path)?
        }
      }
    };
    scratch_outputs.insert(output_name, scratch_path);
  }

  Ok(None)
}
