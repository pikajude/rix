use std::{
  fs::File,
  os::unix::{io::AsRawFd, prelude::RawFd},
  path::Path,
};

use nix::{
  errno::EWOULDBLOCK,
  fcntl::{flock, FlockArg},
};
use users::{os::unix::GroupExt, User};

use super::prelude::*;

pub struct UserLocker {
  group_name: String,
}

impl UserLocker {
  pub fn get() -> Self {
    Self {
      group_name: String::from("nixbld"),
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
          lock: userlock,
        }));
      }
    }
    Ok(None)
  }
}

pub struct UserLock {
  user: User,
  lock: FileWriteLock,
}

impl UserLock {
  pub fn user(&self) -> &User {
    &self.user
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
    if e.as_errno() == Some(EWOULDBLOCK) {
      Ok(false)
    } else {
      Err(e.into())
    }
  } else {
    Ok(true)
  }
}
