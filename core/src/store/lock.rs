#![allow(dead_code)]

use std::fs::File;
use std::os::unix::io::AsRawFd;
use std::os::unix::prelude::RawFd;
use std::path::Path;

use nix::errno::Errno;
use nix::fcntl::{flock, FlockArg};
use nix::sys::signal::{kill, Signal};
use nix::sys::wait::{waitpid, WaitStatus};
use nix::unistd::{fork, setuid, ForkResult, Gid, Pid, Uid};
use users::os::unix::GroupExt;
use users::User;

use super::prelude::*;
use super::settings::settings;

pub struct UserLocker {
  group_name: String,
}

impl UserLocker {
  pub fn get() -> Self {
    Self {
      group_name: String::from(settings().build_users_group()),
    }
  }

  pub fn find(&self) -> Result<Option<UserLock>> {
    let gr = users::get_group_by_name(&self.group_name)
      .ok_or_else(|| anyhow!("the group specified in build-users-group does not exist"))?;
    if gr.members().is_empty() {
      bail!("the build users group '{}' has no members", self.group_name);
    }
    for m in gr.members() {
      let userinfo = users::get_user_by_name(m).ok_or_else(|| {
        anyhow!(
          "the user {:?} in the group '{}' does not exist",
          m,
          self.group_name
        )
      })?;
      if let Some(userlock) =
        FileWriteLock::try_lock(format!("/rix/var/rix/userpool/{}", userinfo.uid()))?
      {
        return Ok(Some(UserLock {
          user: userinfo,
          _lock: userlock,
        }));
      }
    }
    Ok(None)
  }
}

pub struct UserLock {
  user: User,
  _lock: FileWriteLock,
}

impl UserLock {
  pub fn user(&self) -> &User {
    &self.user
  }

  pub fn gid(&self) -> Gid {
    Gid::from_raw(self.user.primary_group_id())
  }

  pub fn uid(&self) -> Uid {
    Uid::from_raw(self.user.uid())
  }

  pub fn kill(self) -> Result<()> {
    debug!("killing all processes running under uid '{}'", self.uid());

    ensure!(
      !self.uid().is_root(),
      "kill() does the wrong thing for uid 0"
    );

    match unsafe { fork()? } {
      ForkResult::Child => {
        setuid(self.uid())?;

        while let Err(errno) = kill(Pid::from_raw(-1), Signal::SIGKILL) {
          if errno == Errno::ESRCH || errno == Errno::EPERM {
            break;
          } else if errno != Errno::EINTR {
            bail!("cannot kill processes for uid '{}'", self.uid());
          }
        }

        std::process::exit(0);
      }
      ForkResult::Parent { child } => match waitpid(child, None) {
        Ok(WaitStatus::Exited(_, _)) => {}
        Ok(w) => bail!("unusual waitpid() output: {:?}", w),
        Err(e) if e != Errno::EINTR => {
          bail!("cannot get child exit status");
        }
        Err(e) => return Err(e.into()),
      },
    }

    Ok(())
  }
}

pub struct FileReadLock {
  _fd: File,
}

impl FileReadLock {
  pub fn try_lock<P: AsRef<Path>>(path: P) -> Result<Option<Self>> {
    let path = path.as_ref();
    let file = File::create(path)?;
    if lock_file(file.as_raw_fd(), FlockArg::LockSharedNonblock)? {
      Ok(Some(Self { _fd: file }))
    } else {
      Ok(None)
    }
  }

  pub fn lock<P: AsRef<Path>>(path: P) -> Result<Self> {
    let path = path.as_ref();
    let file = File::create(path)?;
    lock_file(file.as_raw_fd(), FlockArg::LockShared)?;
    Ok(Self { _fd: file })
  }
}

pub struct FileWriteLock {
  _fd: File,
}

impl FileWriteLock {
  pub fn try_lock<P: AsRef<Path>>(path: P) -> Result<Option<Self>> {
    let path = path.as_ref();
    std::fs::create_dir_all(path.parent().expect("path cannot be empty"))?;
    let file =
      File::create(path).with_context(|| format!("while creating path {}", path.display()))?;
    if lock_file(file.as_raw_fd(), FlockArg::LockExclusiveNonblock)? {
      Ok(Some(Self { _fd: file }))
    } else {
      Ok(None)
    }
  }

  pub fn lock<P: AsRef<Path>>(path: P) -> Result<Self> {
    let path = path.as_ref();
    let file = File::create(path)?;
    lock_file(file.as_raw_fd(), FlockArg::LockExclusive)?;
    Ok(Self { _fd: file })
  }
}

fn lock_file(fd: RawFd, ty: FlockArg) -> Result<bool> {
  if let Err(e) = flock(fd, ty) {
    if e == Errno::EWOULDBLOCK {
      Ok(false)
    } else {
      Err(e.into())
    }
  } else {
    Ok(true)
  }
}
