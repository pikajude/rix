use std::fs::{self, remove_dir, File};
use std::io::{BufRead, BufReader, Write};
use std::os::unix::fs::symlink;
use std::os::unix::prelude::{AsRawFd, RawFd};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::slice;

use super::*;
use crate::store::derivation::output_path_name;
use crate::store::lock::{UserLock, UserLocker};
use crate::store::settings::{settings, SandboxMode};
use crate::store::StorePathSet;

use anyhow::Error;
use crossbeam::thread::Scope;
use libc::SIGCHLD;
use linux_personality::{personality, ADDR_NO_RANDOMIZE};
use nix::errno::Errno;
use nix::fcntl::{open, OFlag};
use nix::mount::{mount, umount2, MntFlags, MsFlags};
use nix::pty::{posix_openpt, ptsname, unlockpt};
use nix::sched::{clone, unshare, CloneFlags};
use nix::sys::mman::{mmap, MapFlags, ProtFlags};
use nix::sys::socket::{socket, AddressFamily, SockFlag, SockType};
use nix::sys::stat::{fchmodat, stat, FchmodatFlags, Mode};
use nix::sys::termios::{cfmakeraw, tcgetattr, tcsetattr, SetArg};
use nix::sys::wait::{waitpid, WaitStatus};
use nix::unistd::*;
use nix::NixPath;
use rlimit::Resource;

const SANDBOX_UID: libc::uid_t = 1000;
const SANDBOX_GID: libc::uid_t = 100;

const NULL: Option<&'static str> = None;

struct ChrootPath {
  path: PathBuf,
  optional: bool,
}

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

struct Build<'a, 'scope, S: Store + ?Sized> {
  store: &'a S,
  messages: &'a Queue<Message>,
  scope: &'a Scope<'scope>,
  drv_path: &'a StorePath,
  drv: &'a Derivation,
  build_user: UserLock,
  env: ProcessEnv,
  input_paths: StorePathSet,
}

impl<'a, 'scope, S: Store + ?Sized> Build<'a, 'scope, S> {
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

  fn run(mut self) -> Result<Option<FinishedChild>> {
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
    for (output_name, status) in initial_outputs {
      let scratch_path = match status.known {
        None => self.fallback_from_output(&output_name)?,
        Some(ref k) => {
          if use_chroot || !k.is_present() || !k.is_valid() {
            k.path.clone()
          } else {
            self.fallback_from_path(&k.path)?
          }
        }
      };
      input_rewrites.insert(
        Hash::placeholder(&output_name),
        self.store.print_store_path(&scratch_path),
      );
      scratch_outputs.insert(output_name, scratch_path.clone());

      let fixed_final_path = match status.known {
        None => continue,
        Some(p) => p.path,
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

    let (mut user_ns_read, mut user_ns_write) = crate::util::pipe::new()?;

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

        let clone_flags = CloneFlags::CLONE_NEWPID
          | CloneFlags::CLONE_NEWNS
          | CloneFlags::CLONE_NEWIPC
          | CloneFlags::CLONE_NEWUTS
          | CloneFlags::CLONE_PARENT
          | CloneFlags::CLONE_NEWNET
          | CloneFlags::CLONE_NEWUSER;

        let mut dirs_in_chroot = Some(dirs_in_chroot);

        info!("about to spawn child");

        let child_pid = clone(
          Box::new(move || {
            match run_child(RunChild {
              builder_write,
              user_ns_read: &mut user_ns_read,
              user_ns_write: &mut user_ns_write,
              chroot_root: &chroot_root,
              chroot_store_dir: &chroot_store_dir,
              dirs_in_chroot: dirs_in_chroot
                .take()
                .expect("function should only run once"),
              tmpdir_in_sandbox,
              drv: self.drv,
              env: &self.env,
              input_rewrites: &input_rewrites,
              build_user: &self.build_user,
            }) {
              Err(e) => {
                eprint!("\x01{:?}\x01", e);
                1
              }
              Ok(_) => 0,
            }
          }),
          stack_slice,
          clone_flags,
          Some(SIGCHLD),
        );

        match child_pid {
          Ok(p) => {
            write(builder_write, format!("{} {}\n", 1, p).as_bytes())?;
            std::process::exit(0)
          }
          Err(e) => {
            error!("unable to spawn child: {:#}", e);
            std::process::exit(1)
          }
        }
      }
      ForkResult::Parent { child } => {
        match waitpid(child, None)? {
          WaitStatus::Exited(_, 0) => {}
          x => bail!("unable to spawn build process: {:?}", x),
        }

        close(user_ns_read.as_raw_fd())?;

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

        let mount_ns = open(
          Path::new(&format!("/proc/{}/ns/mnt", pid)),
          OFlag::O_RDONLY,
          Mode::empty(),
        )?;

        user_ns_write.write_all(b"1")?;
        close(builder_write)?;
        close(user_ns_write.as_raw_fd())?;

        loop {
          line.clear();
          builder_read.read_line(&mut line)?;
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
      }
    }
  }
}

fn do_bind(source: &Path, target: &Path, optional: bool) -> Result<()> {
  let st = match stat(source) {
    Ok(s) => s,
    Err(x) => {
      if x.as_errno() == Some(Errno::ENOENT) && optional {
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

struct RunChild<'a> {
  builder_write: RawFd,
  user_ns_read: &'a mut File,
  user_ns_write: &'a mut File,
  chroot_root: &'a Path,
  chroot_store_dir: &'a Path,
  dirs_in_chroot: HashMap<PathBuf, ChrootPath>,
  tmpdir_in_sandbox: &'a Path,
  drv: &'a Derivation,
  env: &'a HashMap<String, String>,
  input_rewrites: &'a HashMap<String, String>,
  build_user: &'a UserLock,
}

fn run_child(
  RunChild {
    builder_write,
    user_ns_read,
    user_ns_write,
    chroot_root,
    chroot_store_dir,
    mut dirs_in_chroot,
    tmpdir_in_sandbox,
    drv,
    env,
    input_rewrites,
    build_user: _,
  }: RunChild,
) -> Result<()> {
  setsid()?;

  dup2(builder_write, libc::STDERR_FILENO)?;
  dup2(libc::STDERR_FILENO, libc::STDOUT_FILENO)?;
  let fdnull = open("/dev/null", OFlag::O_RDWR, Mode::empty())?;
  dup2(fdnull, libc::STDIN_FILENO)?;
  close(fdnull)?;

  close(user_ns_write.as_raw_fd())?;

  let contents = std::io::read_to_string(user_ns_read)?;
  if contents != "1" {
    return Err(anyhow!("user namespace initialisation failed"));
  }
  close(user_ns_read.as_raw_fd())?;

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
    do_bind(
      &target.path,
      &chroot_root.join(
        source
          .strip_prefix("/")
          .expect("source must start with a slash"),
      ),
      target.optional,
    )?;
  }

  let procfs = chroot_root.join("proc");
  eprintln!("creating procfs at {}", procfs.display());
  fs::create_dir_all(&procfs)?;
  eprintln!("mounting procfs at {}", procfs.display());
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
        if e.as_errno() != Some(Errno::EINVAL) {
          return Err(e.into());
        }
        do_bind(
          Path::new("/dev/pts"),
          chroot_root.join("dev/pts").as_path(),
          false,
        )?;
        do_bind(
          Path::new("/dev/ptmx"),
          chroot_root.join("dev/ptmx").as_path(),
          false,
        )?;
      }
    }
  }

  unshare(CloneFlags::CLONE_NEWNS)?;
  chdir(chroot_root)?;
  mkdir("real-root", Mode::empty())?;
  pivot_root(".", "real-root")?;
  chroot(".")?;
  umount2("real-root", MntFlags::MNT_DETACH)?;
  remove_dir("real-root")?;

  setgid(Gid::from_raw(SANDBOX_GID))?;
  setuid(Uid::from_raw(SANDBOX_UID))?;

  chdir(tmpdir_in_sandbox)?;

  personality(ADDR_NO_RANDOMIZE).map_err(|_| Error::msg("couldn't set personality"))?;

  Resource::CORE.set(0, rlimit::INFINITY)?;

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

fn rewrite(s: &str, rewrites: &HashMap<String, String>) -> String {
  let mut s = s.to_string();
  for (from, to) in rewrites {
    if from == to {
      continue;
    }
    s = s.replace(from, to)
  }
  s
}

pub(super) fn build<S: Store + ?Sized>(
  store: &S,
  messages: &Queue<Message>,
  scope: &Scope<'_>,
  path: &StorePath,
  drv: &Derivation,
) -> Result<Option<FinishedChild>> {
  store.add_temp_root(path)?;

  Build {
    build_user: UserLocker::get()
      .find()?
      .ok_or_else(|| anyhow!("no UIDs available for build"))?,
    store,
    messages,
    scope,
    drv_path: path,
    drv,
    env: Default::default(),
    input_paths: Default::default(),
  }
  .run()
}
