use once_cell::sync::OnceCell;
use rix_settings_macro::Settings;
use std::collections::HashSet;
use std::path::PathBuf;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum SandboxMode {
  On,
  Off,
  Relaxed,
}

impl Default for SandboxMode {
  fn default() -> Self {
    cfg_if::cfg_if! {
      if #[cfg(target_os = "linux")] {
        Self::On
      } else {
        Self::Off
      }
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BuildMode {
  Build,
  Repair,
  Check,
}

impl Default for BuildMode {
  fn default() -> Self {
    Self::Build
  }
}

#[inline]
fn default_paths() -> HashSet<String> {
  maplit::hashset!(
    "/bin/sh=/nix/store/2cxw7z66r105q74f892kns108mqczrqz-bash-5.1-p12-x86_64-unknown-linux-musl/\
     bin/bash"
      .into()
  )
}

#[derive(Debug, Settings)]
pub struct Settings {
  #[setting(default = "nixbld", get = "deref")]
  build_users_group: String,
  #[setting(default_fn = "num_cpus::get")]
  build_cores: usize,
  sandbox_mode: SandboxMode,
  #[setting(default = "/build", get = "deref")]
  sandbox_build_dir: PathBuf,
  #[setting(default_fn = "default_paths", get = "ref")]
  sandbox_paths: HashSet<String>,
  build_mode: BuildMode,
  /// Whether to prevent certain dangerous system calls, such as creation of
  /// setuid/setgid files or adding ACLs or extended attributes. Only disable
  /// this if you're aware of the security implications.
  #[setting(default = true)]
  filter_syscalls: bool,
}

static SETTINGS: OnceCell<Settings> = OnceCell::new();

impl Settings {
  pub fn get() -> &'static Self {
    SETTINGS.get_or_init(Self::default)
  }

  pub fn init_with<F: FnOnce(Self) -> Self>(init_fn: F) {
    if SETTINGS.set(init_fn(Self::default())).is_err() {
      panic!("Settings have already been initialized")
    }
  }
}

pub fn settings() -> &'static Settings {
  Settings::get()
}
