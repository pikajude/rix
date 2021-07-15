use super::*;
use nix::fcntl::OFlag;
use nix::unistd;
use std::fs::File;
use std::io;
use std::os::unix::prelude::{FromRawFd, RawFd};

pub struct PRead {
  fd: RawFd,
  closed: bool,
}

pub struct PWrite {
  fd: RawFd,
  closed: bool,
}

pub fn new() -> Result<(File, File)> {
  let (read_side, write_side) = unistd::pipe2(OFlag::O_CLOEXEC)?;
  Ok(unsafe { (File::from_raw_fd(read_side), File::from_raw_fd(write_side)) })
}

impl PRead {
  pub fn io(&self) -> impl io::Read {
    unsafe { std::fs::File::from_raw_fd(self.fd) }
  }

  pub fn close(&mut self) -> Result<()> {
    self.closed = true;
    Ok(unistd::close(self.fd)?)
  }
}

impl PWrite {
  pub fn io(&self) -> impl io::Write {
    unsafe { std::fs::File::from_raw_fd(self.fd) }
  }

  pub fn close(&mut self) -> Result<()> {
    self.closed = true;
    Ok(unistd::close(self.fd)?)
  }
}

impl Drop for PRead {
  fn drop(&mut self) {
    if !self.closed {
      unistd::close(self.fd).expect("unable to auto-close read side")
    }
  }
}

impl Drop for PWrite {
  fn drop(&mut self) {
    if !self.closed {
      unistd::close(self.fd).expect("unable to auto-close pipe")
    }
  }
}
