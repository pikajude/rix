use once_cell::sync::OnceCell;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum SandboxMode {
  On,
  Off,
  Relaxed,
}

impl Default for SandboxMode {
  #[cfg(target_os = "linux")]
  fn default() -> Self {
    Self::On
  }

  #[cfg(not(target_os = "linux"))]
  fn default() -> Self {
    Self::Off
  }
}

#[derive(Debug)]
pub struct Settings {
  build_users_group: Option<String>,
  sandbox_mode: SandboxMode,
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

  pub fn build_users_group(&self) -> Option<&str> {
    self.build_users_group.as_deref()
  }

  pub fn sandbox_mode(&self) -> SandboxMode {
    self.sandbox_mode
  }
}

impl Default for Settings {
  fn default() -> Self {
    Self {
      build_users_group: Some("nixbld".into()),
      sandbox_mode: SandboxMode::default(),
    }
  }
}

pub fn settings() -> &'static Settings {
  Settings::get()
}
