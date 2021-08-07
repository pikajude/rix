use std::borrow::Cow;
use std::io::{BufRead, BufReader};
use std::os::unix::fs::symlink;
use std::os::unix::prelude::RawFd;
use std::path::{Path, PathBuf};
use std::process::{exit, Command};
use std::{fs, slice};

use super::*;
use crate::fetch::builtin_fetchurl;
use crate::store::derivation::output_path_name;
use crate::store::lock::{UserLock, UserLocker};
use crate::store::settings::{settings, BuildMode, SandboxMode};
use crate::store::StorePathSet;
mod sys_ext;

use anyhow::Error;
use crossbeam::thread::Scope;
use ipc_channel::ipc::IpcBytesReceiver;
use libc::SIGCHLD;
use linux_personality::{personality, ADDR_NO_RANDOMIZE};
use nix::errno::Errno;
use nix::fcntl::{open, OFlag};
use nix::mount::{mount, umount2, MntFlags, MsFlags};
use nix::pty::{posix_openpt, ptsname, unlockpt};
use nix::sched::{unshare, CloneFlags};
use nix::sys::mman::{mmap, MapFlags, ProtFlags};
use nix::sys::socket::{socket, AddressFamily, SockFlag, SockType};
use nix::sys::stat::{fchmodat, stat, FchmodatFlags, Mode};
use nix::sys::termios::{cfmakeraw, tcgetattr, tcsetattr, SetArg};
use nix::sys::wait::{waitpid, WaitStatus};
use nix::unistd::*;
use nix::NixPath;
use parking_lot::Once;
use rlimit::Resource;

const SANDBOX_UID: Uid = Uid::from_raw(1000);
const SANDBOX_GID: Gid = Gid::from_raw(100);

const NULL: Option<&'static str> = None;

struct ChrootPath {
  path: PathBuf,
  optional: bool,
}

#[allow(dead_code)]
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

type ProcessEnv = HashMap<String, String>;

struct Build<'a, S: Store + ?Sized> {
  store: &'a S,
  drv_path: &'a StorePath,
  drv: &'a Derivation,
  build_user: UserLock,
  env: ProcessEnv,
  input_paths: StorePathSet,
}

impl<'a, S: Store + ?Sized> Build<'a, S> {
  fn fallback_from_output(&self, output_name: &str) -> Result<StorePath> {
    self.store.make_store_path(
      &format!("rewrite:{}:name:{}", self.drv_path.to_string(), output_name),
      Hash::new_allow_empty("", Some(HashType::SHA256))?,
      &output_path_name(&self.drv.name, output_name),
    )
  }

  fn fallback_from_path(&self, path: &StorePath) -> Result<StorePath> {
    self.store.make_store_path(
      &format!("rewrite:{}:{}", self.drv_path.to_string(), path.to_string()),
      Hash::new_allow_empty("", Some(HashType::SHA256))?,
      path.name(),
    )
  }

  fn static_hashes(&self) -> Result<Box<dyn Iterator<Item = (String, Hash)> + 'a>> {
    match self.store.hash_derivation_modulo(self.drv, true)? {
      HashModulo::Known(h) | HashModulo::Deferred(h) => Ok(Box::new(
        self.drv.outputs.keys().map(move |x| (x.to_string(), h)),
      )),
      HashModulo::FixedOutput(e) => Ok(Box::new(e.into_iter())),
    }
  }

  fn chown_to_builder<P: AsRef<Path>>(&self, path: P) -> Result<()> {
    let path = path.as_ref();
    chown(
      path,
      Some(self.build_user.uid()),
      Some(self.build_user.gid()),
    )
    .with_context(|| {
      format!(
        "unable to chown directory {} to uid:gid {}:{}",
        path.display(),
        self.build_user.uid(),
        self.build_user.gid()
      )
    })
  }

  fn init_tmp_dir(&mut self, build_tmp_dir: &Path) -> Result<()> {
    let tmpdir_in_sandbox = settings().sandbox_build_dir();
    let pass_as_file = self
      .drv
      .env
      .get("passAsFile")
      .map_or("", |x| x.as_str())
      .split_ascii_whitespace()
      .collect::<HashSet<_>>();

    for (key, value) in &self.drv.env {
      if pass_as_file.contains(&**key) {
        let h = Hash::hash(key, HashType::SHA256);
        let filename = format!(".attr-{}", h.encode(Encoding::Base32));
        let filepath = build_tmp_dir.join(&filename);
        fs::write(&filepath, value)?;
        self.env.insert(
          format!("{}Path", key),
          tmpdir_in_sandbox.join(filename).display().to_string(),
        );
      } else {
        self.env.insert(key.to_string(), value.into());
      }
    }

    self.env.insert(
      "NIX_BUILD_TOP".into(),
      tmpdir_in_sandbox.display().to_string(),
    );
    for tmp in &["TMPDIR", "TEMPDIR", "TMP", "TEMP", "PWD"] {
      self
        .env
        .insert((*tmp).into(), tmpdir_in_sandbox.display().to_string());
    }

    Ok(())
  }

  fn run(mut self) -> Result<()> {
    static NSS_INIT: Once = Once::new();

    if self.drv.is_builtin() {
      NSS_INIT.call_once(|| {
        let res = dns_lookup::getaddrinfo(Some("invalid-domain.invalid"), Some("http"), None);
        assert!(res.is_err());
      });
    }

    let build_mode = settings().build_mode();

    for (drv_path, wanted) in self.drv.input_derivations.iter() {
      let outs = self
        .store
        .try_read_derivation(drv_path)?
        .outputs_and_opt_paths(self.store)?;
      for j in wanted {
        match outs.get(j) {
          Some((_, Some(realized_input))) => {
            self.store.compute_fs_closure(
              realized_input,
              &mut self.input_paths,
              Default::default(),
            )?;
          }
          Some((_, None)) => bail!(
            "derivation '{}' requires un-realised output '{}' from input derivation '{}'",
            self.store.print_store_path(self.drv_path),
            j,
            self.store.print_store_path(drv_path)
          ),
          None => bail!(
            "derivation '{}' requires nonexistent output '{}' from input derivation '{}'",
            self.store.print_store_path(self.drv_path),
            j,
            self.store.print_store_path(drv_path)
          ),
        }
      }
    }

    for p in &self.drv.input_sources {
      self
        .store
        .compute_fs_closure(p, &mut self.input_paths, Default::default())?;
    }

    debug!("adding input paths {:?}", self.input_paths);

    let initial_outputs = self
      .static_hashes()?
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

    let build_log_path = self.store.log_file_of(self.drv_path);

    fs::create_dir_all(build_log_path.parent().unwrap())?;

    let no_chroot = self.drv.env.get("__noChroot").map_or(false, |x| x == "1");
    let use_chroot;
    if settings().sandbox_mode() == SandboxMode::On {
      if no_chroot {
        bail!(
          "derivation '{}' has __noChroot set, which is not allowed",
          self.store.print_store_path(self.drv_path)
        );
      }
      use_chroot = true;
    } else if settings().sandbox_mode() == SandboxMode::Off {
      use_chroot = false;
    } else {
      use_chroot = !no_chroot;
    }

    let build_tmp_dir = tempfile::Builder::new()
      .prefix(&format!("nix-build-{}-", &self.drv.name))
      .tempdir()?;

    self.chown_to_builder(build_tmp_dir.path())?;

    let mut input_rewrites = HashMap::new();
    let mut scratch_outputs = HashMap::new();
    let mut redirected_outputs = HashMap::new();
    for (output_name, status) in initial_outputs.iter() {
      let scratch_path = match status.known {
        None => self.fallback_from_output(output_name)?,
        Some(ref k) => {
          if use_chroot || !k.is_present() || (build_mode != BuildMode::Repair && !k.is_valid()) {
            k.path.clone()
          } else {
            self.fallback_from_path(&k.path)?
          }
        }
      };
      input_rewrites.insert(
        Hash::placeholder(output_name),
        self.store.print_store_path(&scratch_path),
      );
      scratch_outputs.insert(output_name.clone(), scratch_path.clone());

      let fixed_final_path = match status.known {
        None => continue,
        Some(ref p) => p.path.clone(),
      };

      if fixed_final_path == scratch_path {
        continue;
      }

      rm_rf::ensure_removed(self.store.print_store_path(&scratch_path))?;

      input_rewrites.insert(fixed_final_path.hash_part(), scratch_path.hash_part());

      redirected_outputs.insert(fixed_final_path, scratch_path);
    }

    self.env.insert("PATH".into(), "/path-not-set".into());
    self.env.insert("HOME".into(), "/homeless-shelter".into());
    self.env.insert(
      "NIX_STORE".into(),
      self.store.store_path().display().to_string(),
    );
    self.env.insert(
      "NIX_BUILD_CORES".into(),
      settings().build_cores().to_string(),
    );

    self.init_tmp_dir(build_tmp_dir.path())?;

    if self.drv.is_fixed() {
      self.env.insert("NIX_OUTPUT_CHECKED".into(), "1".into());
    }

    if self.drv.is_impure() {
      for var in self
        .drv
        .env
        .get("impureEnvVars")
        .map_or("", |x| x.as_str())
        .split_ascii_whitespace()
      {
        self
          .env
          .insert(var.into(), std::env::var(var).unwrap_or_default());
      }
    }

    self.env.insert("NIX_LOG_FD".into(), "2".into());
    self.env.insert("TERM".into(), "xterm-256color".into());

    let mut dirs_in_chroot = HashMap::new();
    if use_chroot {
      for g in settings().sandbox_paths().iter() {
        let mut optional = false;
        let g = if let Some(g2) = g.strip_suffix('?') {
          optional = true;
          g2
        } else {
          g.as_str()
        };
        match break_str(g, '=') {
          Some((src, dest)) => {
            dirs_in_chroot.insert(
              PathBuf::from(src),
              ChrootPath {
                path: PathBuf::from(dest),
                optional,
              },
            );
          }
          None => {
            dirs_in_chroot.insert(
              PathBuf::from(g),
              ChrootPath {
                path: PathBuf::from(g),
                optional,
              },
            );
          }
        }
      }
    }
    let tmpdir_in_sandbox = settings().sandbox_build_dir();
    dirs_in_chroot.insert(
      PathBuf::from(tmpdir_in_sandbox),
      ChrootPath {
        path: build_tmp_dir.path().to_path_buf(),
        optional: false,
      },
    );

    let mut paths = StorePathSet::new();
    for dir in dirs_in_chroot.values() {
      if self.store.is_in_store(&dir.path) {
        self.store.compute_fs_closure(
          &self.store.parse_store_path(&dir.path)?,
          &mut paths,
          Default::default(),
        )?;
      }
    }
    for cl in paths {
      let real = self.store.print_store_path(&cl);
      dirs_in_chroot.insert(
        PathBuf::from(&real),
        ChrootPath {
          path: real.into(),
          optional: false,
        },
      );
    }

    let chroot_root = self
      .store
      .to_real_path(self.drv_path)
      .with_extension("drv.chroot");
    rm_rf::ensure_removed(&chroot_root)?;

    debug!("setting up chroot environment in {}", chroot_root.display());

    mkdir(&chroot_root, Mode::from_bits_truncate(0o750))?;

    chown(&chroot_root, None, Some(self.build_user.gid()))?;
    let chroot_tmp = chroot_root.join("tmp");
    fs::create_dir_all(&chroot_tmp)?;
    chmod(&chroot_tmp, 0o1777)?;

    fs::create_dir_all(chroot_root.join("etc"))?;
    fs::write(
      chroot_root.join("etc/group"),
      format!("root:x:0:\nnixbld:!:{}:\nnogroup:x:65534:\n", SANDBOX_GID),
    )?;

    if !self.drv.is_impure() {
      fs::write(
        chroot_root.join("etc/hosts"),
        "127.0.0.1 localhost\n::1 localhost\n",
      )?;
    }

    let chroot_store_dir = chroot_root.join(
      self
        .store
        .store_path()
        .strip_prefix("/")
        .expect("store_path should be absolute"),
    );
    fs::create_dir_all(&chroot_store_dir)?;
    chmod(&chroot_store_dir, 0o1775)?;
    chown(&chroot_store_dir, None, Some(self.build_user.gid()))?;

    info!("executing builder {}", self.drv.builder.display());

    let builder_read = posix_openpt(OFlag::O_RDWR | OFlag::O_NOCTTY)?;
    let slave_name = unsafe { ptsname(&builder_read) }?;

    chmod(&*slave_name, 0o600)?;
    chown(&*slave_name, Some(self.build_user.uid()), None)?;

    unlockpt(&builder_read)?;

    let builder_write = open(&*slave_name, OFlag::O_RDWR | OFlag::O_NOCTTY, Mode::empty())?;

    let mut term = tcgetattr(builder_write)?;
    cfmakeraw(&mut term);
    tcsetattr(builder_write, SetArg::TCSANOW, &term)?;

    let private_network = !self.drv.is_impure();

    let (user_ns_write, user_ns_read) = ipc_channel::ipc::bytes_channel()?;

    match unsafe { fork() }? {
      ForkResult::Child => {
        if getuid().is_root() {
          setgroups(&[])?;
        }

        let stack_size = 1024 * 1024 * 8;
        let stack = unsafe {
          mmap(
            std::ptr::null_mut(),
            stack_size,
            ProtFlags::PROT_WRITE | ProtFlags::PROT_READ,
            MapFlags::MAP_PRIVATE | MapFlags::MAP_ANONYMOUS | MapFlags::MAP_STACK,
            -1,
            0,
          )
        }?;
        let stack_slice = unsafe { slice::from_raw_parts_mut(stack.cast::<u8>(), stack_size) };

        let mut clone_flags = CloneFlags::CLONE_NEWPID
          | CloneFlags::CLONE_NEWNS
          | CloneFlags::CLONE_NEWIPC
          | CloneFlags::CLONE_NEWUTS
          | CloneFlags::CLONE_PARENT
          | CloneFlags::CLONE_NEWUSER;

        if private_network {
          clone_flags.set(CloneFlags::CLONE_NEWNET, true);
        }

        let child_pid = sys_ext::clone(
          move || match run_child(RunChild {
            builder_write,
            user_ns_read,
            chroot_root: &chroot_root,
            chroot_store_dir: &chroot_store_dir,
            dirs_in_chroot,
            tmpdir_in_sandbox,
            private_network,
            drv: self.drv,
            env: &self.env,
            input_rewrites: &input_rewrites,
          }) {
            Err(e) => {
              eprint!("\x01{:?}\x01", e);
              1
            }
            Ok(_) => 0,
          },
          stack_slice,
          clone_flags,
          Some(SIGCHLD),
        );

        match child_pid {
          Ok(p) => {
            write(builder_write, format!("{} {}\n", 1, p).as_bytes())?;
            exit(0)
          }
          Err(e) => {
            error!("unable to spawn child: {:#}", e);
            exit(1)
          }
        }
      }
      ForkResult::Parent { child } => {
        match waitpid(child, None)? {
          WaitStatus::Exited(_, 0) => {}
          x => bail!("unable to spawn build process: {:?}", x),
        }

        let mut line = String::new();
        let mut builder_read = BufReader::new(builder_read);
        builder_read.read_line(&mut line)?;

        let words = line.split_ascii_whitespace().collect::<Vec<_>>();
        let using_user_ns = words[0] == "1";
        let pid = Pid::from_raw(words[1].parse()?);

        if using_user_ns {
          let host_uid = self.build_user.uid();
          let host_gid = self.build_user.gid();

          fs::write(
            format!("/proc/{}/uid_map", pid),
            format!("{} {} 1", SANDBOX_UID, host_uid).as_bytes(),
          )?;
          fs::write(format!("/proc/{}/setgroups", pid), b"deny")?;
          fs::write(
            format!("/proc/{}/gid_map", pid),
            format!("{} {} 1", SANDBOX_GID, host_gid).as_bytes(),
          )?;
        } else {
          bail!("not using a user namespace");
        }

        fs::write(
          chroot_root.join("etc/passwd"),
          format!(
            "root:x:0:0:Nix build user:{dir}:/noshell\nnixbld:x:{uid}:{gid}:Nix build \
             user:{dir}:/noshell\nnobody:x:65534:65534:Nobody:/:/noshell\n",
            dir = tmpdir_in_sandbox.display(),
            uid = SANDBOX_UID,
            gid = SANDBOX_GID
          )
          .as_bytes(),
        )?;

        let _mount_ns = open(
          Path::new(&format!("/proc/{}/ns/mnt", pid)),
          OFlag::O_RDONLY,
          Mode::empty(),
        )?;

        user_ns_write.send(&[1])?;
        close(builder_write)?;
        drop(user_ns_write);

        loop {
          line.clear();
          match builder_read.read_line(&mut line) {
            Err(e) if e.raw_os_error() == Some(libc::EIO) => break,
            Ok(0) => break,
            Ok(_) => {}
            Err(e) => return Err(e.into()),
          }
          if let Some(desc) = line.strip_prefix('\x01') {
            let mut full_contents = vec![];
            let amt_read = builder_read.read_until(1, &mut full_contents)?;
            let err_string = std::str::from_utf8(&full_contents[..amt_read - 1])?;
            bail!(
              "while setting up the build environment: {}{}",
              desc,
              err_string
            );
          }
          debug!("sandbox setup: {}", line.trim_end());
        }

        // at this point, builder terminated without throwing an error, so assume the
        // build succeeded
        let mut referenceable_paths = HashSet::new();
        referenceable_paths.extend(self.input_paths.iter());
        referenceable_paths.extend(scratch_outputs.values());

        for outname in self.drv.outputs.keys() {
          let actual_path = cat_paths(
            &chroot_root,
            self.store.print_store_path(&scratch_outputs[outname]),
          );

          let out_path_mode = stat(&actual_path).with_context(|| {
            format!(
              "builder for '{}' failed to produce output path for output '{}' at '{}'",
              self.store.print_store_path(self.drv_path),
              outname,
              actual_path.display()
            )
          })?;

          debug!(
            "scanning for references for output '{}' in temp location '{}' with mode {:?}",
            outname,
            actual_path.display(),
            out_path_mode
          );

          // TODO: canonicalize

          let _found = crate::store::refs::scan_for_references(
            &actual_path,
            referenceable_paths.iter().copied(),
          )?;
        }

        Ok(())
      }
    }
  }
}

fn do_bind<P: AsRef<Path>, Q: AsRef<Path>>(source: P, target: Q, optional: bool) -> Result<()> {
  let source = source.as_ref();
  let target = target.as_ref();
  let st = match stat(source) {
    Ok(s) => s,
    Err(x) => {
      if x == Errno::ENOENT && optional {
        return Ok(());
      } else {
        return Err(x.into());
      }
    }
  };
  eprintln!(
    "bind mounting '{}' to '{}'",
    source.display(),
    target.display()
  );
  if st.st_mode & libc::S_IFMT == libc::S_IFDIR {
    fs::create_dir_all(target)?;
  } else {
    fs::create_dir_all(
      target
        .parent()
        .expect("bind mount path must have at least one element"),
    )?;
    fs::write(target, "")?;
  }
  mount(
    Some(source),
    target,
    NULL,
    MsFlags::MS_BIND | MsFlags::MS_REC,
    NULL,
  )?;
  Ok(())
}

fn chmod<P: NixPath + ?Sized>(path: &P, mode: u32) -> Result<(), nix::Error> {
  fchmodat(
    None,
    path,
    Mode::from_bits_truncate(mode),
    FchmodatFlags::FollowSymlink,
  )
}

fn init_seccomp() -> Result<()> {
  use scmp_compare::SCMP_CMP_MASKED_EQ;
  use seccomp_sys::*;
  use std::ops::Deref;

  struct Dealloc(*mut libc::c_void);

  impl Drop for Dealloc {
    fn drop(&mut self) {
      unsafe { seccomp_release(self.0) }
    }
  }

  impl Deref for Dealloc {
    type Target = *mut libc::c_void;

    fn deref(&self) -> &Self::Target {
      &self.0
    }
  }

  unsafe {
    let ctx = seccomp_init(SCMP_ACT_ALLOW);
    if ctx.is_null() {
      bail!(Errno::last());
    }

    let ctx = Dealloc(ctx);

    for perm in &[libc::S_ISUID, libc::S_ISGID] {
      Errno::result(seccomp_rule_add(
        *ctx,
        SCMP_ACT_ERRNO(libc::EPERM as _),
        libc::SYS_chmod as _,
        1,
        scmp_arg_cmp {
          arg: 1,
          op: SCMP_CMP_MASKED_EQ,
          datum_a: *perm as _,
          datum_b: *perm as _,
        },
      ))?;

      Errno::result(seccomp_rule_add(
        *ctx,
        SCMP_ACT_ERRNO(libc::EPERM as _),
        libc::SYS_fchmod as _,
        1,
        scmp_arg_cmp {
          arg: 1,
          op: SCMP_CMP_MASKED_EQ,
          datum_a: *perm as _,
          datum_b: *perm as _,
        },
      ))?;

      Errno::result(seccomp_rule_add(
        *ctx,
        SCMP_ACT_ERRNO(libc::EPERM as _),
        libc::SYS_fchmodat as _,
        1,
        scmp_arg_cmp {
          arg: 2,
          op: SCMP_CMP_MASKED_EQ,
          datum_a: *perm as _,
          datum_b: *perm as _,
        },
      ))?;
    }

    Errno::result(seccomp_rule_add(
      *ctx,
      SCMP_ACT_ERRNO(libc::ENOTSUP as _),
      libc::SYS_setxattr as _,
      0,
    ))?;
    Errno::result(seccomp_rule_add(
      *ctx,
      SCMP_ACT_ERRNO(libc::ENOTSUP as _),
      libc::SYS_lsetxattr as _,
      0,
    ))?;
    Errno::result(seccomp_rule_add(
      *ctx,
      SCMP_ACT_ERRNO(libc::ENOTSUP as _),
      libc::SYS_fsetxattr as _,
      0,
    ))?;

    Errno::result(seccomp_attr_set(
      *ctx,
      scmp_filter_attr::SCMP_FLTATR_CTL_NNP,
      0,
    ))?;

    Errno::result(seccomp_load(*ctx))?;
  }

  Ok(())
}

struct RunChild<'a> {
  builder_write: RawFd,
  user_ns_read: IpcBytesReceiver,
  chroot_root: &'a Path,
  chroot_store_dir: &'a Path,
  dirs_in_chroot: HashMap<PathBuf, ChrootPath>,
  tmpdir_in_sandbox: &'a Path,
  private_network: bool,
  drv: &'a Derivation,
  env: &'a HashMap<String, String>,
  input_rewrites: &'a HashMap<String, String>,
}

fn run_child(
  RunChild {
    builder_write,
    user_ns_read,
    chroot_root,
    chroot_store_dir,
    mut dirs_in_chroot,
    tmpdir_in_sandbox,
    drv,
    env,
    input_rewrites,
    private_network,
  }: RunChild,
) -> Result<()> {
  setsid()?;

  dup2(builder_write, libc::STDERR_FILENO)?;
  dup2(libc::STDERR_FILENO, libc::STDOUT_FILENO)?;
  let fdnull = open("/dev/null", OFlag::O_RDWR, Mode::empty())?;
  dup2(fdnull, libc::STDIN_FILENO)?;
  close(fdnull)?;

  init_seccomp()?;

  let contents = user_ns_read.recv()?;
  ensure!(contents == [1], "user namespace initialisation failed");
  drop(user_ns_read);

  if private_network {
    let sock = socket(
      AddressFamily::Inet,
      SockType::Datagram,
      SockFlag::empty(),
      None,
    )?;
    netdevice::set_flags(
      sock,
      "lo",
      &(netdevice::IFF_UP | netdevice::IFF_LOOPBACK | netdevice::IFF_RUNNING),
    )?;
  }

  sethostname("localhost")?;

  mount(NULL, "/", NULL, MsFlags::MS_PRIVATE | MsFlags::MS_REC, NULL)?;

  mount(Some(chroot_root), chroot_root, NULL, MsFlags::MS_BIND, NULL)?;

  mount(
    Some(chroot_store_dir),
    chroot_store_dir,
    NULL,
    MsFlags::MS_BIND,
    NULL,
  )?;

  mount(NULL, chroot_store_dir, NULL, MsFlags::MS_SHARED, NULL)
    .with_context(|| format!("unable to mount {} as shared", chroot_store_dir.display()))?;

  let mut other_paths = vec![];
  if !dirs_in_chroot.contains_key(Path::new("/dev")) {
    fs::create_dir_all(chroot_root.join("dev/shm"))?;
    fs::create_dir_all(chroot_root.join("dev/pts"))?;
    other_paths.push("/dev/full");
    if Path::new("/dev/kvm").exists() {
      other_paths.push("/dev/kvm");
    }
    other_paths.extend(vec![
      "/dev/null",
      "/dev/random",
      "/dev/tty",
      "/dev/urandom",
      "/dev/zero",
    ]);
    symlink("/proc/self/fd", chroot_root.join("dev/fd"))?;
    symlink("/proc/self/fd/0", chroot_root.join("dev/stdin"))?;
    symlink("/proc/self/fd/1", chroot_root.join("dev/stdout"))?;
    symlink("/proc/self/fd/2", chroot_root.join("dev/stderr"))?;
  }

  if drv.is_impure() {
    fs::write(
      chroot_root.join("etc/nsswitch.conf"),
      b"hosts: files dns\nservices: files\n",
    )?;
    for path in [
      "/etc/resolv.conf",
      "/etc/services",
      "/etc/hosts",
      "/var/run/nscd/socket",
    ]
    .iter()
    {
      if Path::new(path).exists() {
        other_paths.push(*path);
      }
    }
  }

  for path in other_paths {
    dirs_in_chroot.insert(
      PathBuf::from(path),
      ChrootPath {
        path: PathBuf::from(path),
        optional: false,
      },
    );
  }

  let has_pts = dirs_in_chroot.contains_key(Path::new("/dev/pts"));

  for (source, target) in dirs_in_chroot.drain() {
    if target.path == Path::new("/proc") {
      continue;
    }
    do_bind(target.path, cat_paths(chroot_root, source), target.optional)?;
  }

  let procfs = chroot_root.join("proc");
  fs::create_dir_all(&procfs)?;
  mount(
    Some("none"),
    procfs.as_path(),
    Some("proc"),
    MsFlags::empty(),
    NULL,
  )?;

  if Path::new("/dev/shm").exists() {
    mount(
      Some("none"),
      chroot_root.join("dev/shm").as_path(),
      Some("tmpfs"),
      MsFlags::empty(),
      Some("size=50%"),
    )?;
  }

  if Path::new("/dev/pts/ptmx").exists() && !chroot_root.join("dev/ptmx").exists() && !has_pts {
    let res = mount(
      Some("none"),
      chroot_root.join("dev/pts").as_path(),
      Some("devpts"),
      MsFlags::empty(),
      Some("newinstance,mode=0620"),
    );
    match res {
      Ok(_) => {
        symlink("/dev/pts/ptmx", chroot_root.join("dev/ptmx"))?;
        chmod(chroot_root.join("/dev/ptmx").as_path(), 0o666)?;
      }
      Err(e) => {
        if e != Errno::EINVAL {
          return Err(e.into());
        }
        do_bind("/dev/pts", chroot_root.join("dev/pts"), false)?;
        do_bind("/dev/ptmx", chroot_root.join("dev/ptmx"), false)?;
      }
    }
  }

  #[cfg(debug_assertions)]
  {
    // libunwind opens /proc/self/exe to resolve symbols. the path it points to
    // normally doesn't exist in the sandbox, so this bind mount makes it accessible
    let real_self = fs::read_link("/proc/self/exe")?;
    do_bind(&real_self, cat_paths(chroot_root, &real_self), false)?;
  }

  unshare(CloneFlags::CLONE_NEWNS)?;
  chdir(chroot_root)?;
  mkdir("real-root", Mode::empty())?;
  pivot_root(".", "real-root")?;
  chroot(".")?;
  umount2("real-root", MntFlags::MNT_DETACH)?;
  fs::remove_dir("real-root")?;

  setgid(SANDBOX_GID)?;
  setuid(SANDBOX_UID)?;

  chdir(tmpdir_in_sandbox)?;

  personality(ADDR_NO_RANDOMIZE).map_err(|_| Error::msg("couldn't set personality"))?;

  Resource::CORE.set(0, rlimit::INFINITY)?;

  if let Some(b) = drv.as_builtin() {
    let new_env = drv
      .env
      .iter()
      .map(|(k, v)| (k.to_string(), rewrite(v, input_rewrites)))
      .collect::<HashMap<_, _>>();

    match b.as_str() {
      "fetchurl" => builtin_fetchurl(&new_env),
      x => bail!("unsupported builtin {}", x),
    }
  } else {
    let mut builder = Command::new(&drv.builder);

    for (var, value) in env {
      builder.env(var, rewrite(value, input_rewrites));
    }

    for arg in &drv.args {
      builder.arg(rewrite(arg, input_rewrites));
    }

    eprintln!("executing builder: {:?}", builder);

    let st = builder.status()?;
    ensure!(st.success(), "non-zero exit status: {}", st);
    Ok(())
  }
}

fn rewrite(s: &str, rewrites: &HashMap<String, String>) -> String {
  let mut s = Cow::Borrowed(s);
  for (from, to) in rewrites {
    if from == to {
      continue;
    }
    s = Cow::Owned(s.replace(from, to));
  }
  s.into_owned()
}

pub(super) fn build<S: Store + ?Sized>(
  store: &S,
  _: &Scope<'_>,
  path: &StorePath,
  drv: &Derivation,
) -> Result<()> {
  store.add_temp_root(path)?;

  Build {
    build_user: UserLocker::get()
      .find()?
      .ok_or_else(|| anyhow!("no UIDs available for build"))?,
    store,
    drv_path: path,
    drv,
    env: Default::default(),
    input_paths: Default::default(),
  }
  .run()
}
