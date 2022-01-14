use once_cell::sync::OnceCell;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

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

#[derive(Debug)]
pub struct Settings {
  build_users_group: String,
  build_cores: usize,
  sandbox_mode: SandboxMode,
  sandbox_build_dir: PathBuf,
  sandbox_paths: HashSet<String>,
  build_mode: BuildMode,
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

  pub fn build_users_group(&self) -> &str {
    self.build_users_group.as_str()
  }

  pub fn sandbox_mode(&self) -> SandboxMode {
    self.sandbox_mode
  }

  pub fn build_cores(&self) -> usize {
    self.build_cores
  }

  pub fn build_mode(&self) -> BuildMode {
    self.build_mode
  }

  pub fn sandbox_build_dir(&self) -> &Path {
    &self.sandbox_build_dir
  }

  pub fn sandbox_paths(&self) -> &HashSet<String> {
    &self.sandbox_paths
  }
}

impl Default for Settings {
  fn default() -> Self {
    Self {
      build_users_group: "nixbld".into(),
      build_cores: num_cpus::get(),
      sandbox_mode: SandboxMode::default(),
      sandbox_build_dir: PathBuf::from("/build"),
      sandbox_paths: maplit::hashset! {},
      build_mode: BuildMode::default(),
    }
  }
}

pub fn settings() -> &'static Settings {
  Settings::get()
}
