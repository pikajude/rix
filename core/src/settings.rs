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
  /// This options specifies the Unix group containing the Nix build user
  /// accounts. In multi-user Nix installations, builds should not be performed
  /// by the Nix account since that would allow users to arbitrarily modify the
  /// Nix store and database by supplying specially crafted builders; and they
  /// cannot be performed by the calling user since that would allow him/her to
  /// influence the build result.
  ///
  /// Therefore, if this option is non-empty and specifies a valid group, builds
  /// will be performed under the user accounts that are a member of the group
  /// specified here (as listed in `/etc/group`). Those user accounts should not
  /// be used for any other purpose!
  ///
  /// Nix will never run two builds under the same user account at the same
  /// time. This is to prevent an obvious security hole: a malicious user
  /// writing a Nix expression that modifies the build result of a legitimate
  /// Nix expression being built by another user. Therefore it is good to have
  /// as many Nix build user accounts as you can spare.  (Remember: uids are
  /// cheap.)
  ///
  /// The build users should have permission to create files in the Nix store,
  /// but not delete them. Therefore, `/nix/store` should be owned by the Nix
  /// account, its group should be the group specified here, and its mode should
  /// be `1775`.
  ///
  /// If the build users group is empty, builds will be performed under the uid
  /// of the Nix process (that is, the uid of the caller if `NIX_REMOTE` is
  /// empty, the uid under which the Nix daemon runs if `NIX_REMOTE` is
  /// `daemon`). Obviously, this should not be used in multi-user settings with
  /// untrusted users.
  #[setting(default = "nixbld", get = "deref")]
  build_users_group: String,

  /// Sets the value of the `NIX_BUILD_CORES` environment variable in the
  /// invocation of builders. Builders can use this variable at their discretion
  /// to control the maximum amount of parallelism. For instance, in Nixpkgs, if
  /// the derivation attribute `enableParallelBuilding` is set to `true`, the
  /// builder passes the `-jN` flag to GNU Make. It can be overridden using the
  /// `--cores` command line switch and defaults to `1`. The value `0` means
  /// that the builder should use all available CPU cores in the system.
  #[setting(default_fn = "num_cpus::get")]
  build_cores: usize,

  /// If set to `true`, builds will be performed in a *sandboxed environment*,
  /// i.e., they’re isolated from the normal file system hierarchy and will only
  /// see their dependencies in the Nix store, the temporary build directory,
  /// private versions of `/proc`, `/dev`, `/dev/shm` and `/dev/pts` (on Linux),
  /// and the paths configured with the `sandbox-paths` option. This is useful
  /// to prevent undeclared dependencies on files in directories such as
  /// `/usr/bin`. In addition, on Linux, builds run in private PID, mount,
  /// network, IPC and UTS namespaces to isolate them from other processes in
  /// the system (except that fixed-output derivations do not run in private
  /// network namespace to ensure they can access the network).
  ///
  /// Currently, sandboxing only work on Linux and macOS. The use of a sandbox
  /// requires that Nix is run as root (so you should use the “build users”
  /// feature to perform the actual builds under different users than root).
  ///
  /// If this option is set to `relaxed`, then fixed-output derivations and
  /// derivations that have the `__noChroot` attribute set to `true` do not run
  /// in sandboxes.
  ///
  /// The default is `true` on Linux and `false` on all other platforms.
  sandbox_mode: SandboxMode,

  /// The build directory inside the sandbox.
  #[setting(default = "/build", get = "deref")]
  sandbox_build_dir: PathBuf,

  /// A list of paths bind-mounted into Nix sandbox environments. You can use
  /// the syntax `target=source` to mount a path in a different location in the
  /// sandbox; for instance, `/bin=/nix-bin` will mount the path `/nix-bin` as
  /// `/bin` inside the sandbox. If *source* is followed by `?`, then it is not
  /// an error if *source* does not exist; for example, `/dev/nvidiactl?`
  /// specifies that `/dev/nvidiactl` will only be mounted in the sandbox if it
  /// exists in the host filesystem.
  ///
  /// Depending on how Nix was built, the default value for this option may be
  /// empty or provide `/bin/sh` as a bind-mount of `bash`.
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
