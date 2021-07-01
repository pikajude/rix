use super::*;
use nix::{fcntl::OFlag, unistd};
use std::{io, os::unix::prelude::RawFd};

pub struct PipeRead(RawFd);

pub struct PipeWrite(RawFd);

pub fn new() -> Result<(PipeRead, PipeWrite)> {
  let (read_side, write_side) = unistd::pipe2(OFlag::O_CLOEXEC)?;
  Ok((PipeRead(read_side), PipeWrite(write_side)))
}

impl io::Write for PipeWrite {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    unistd::write(self.0, buf).map_err(|e| io::Error::new(io::ErrorKind::Other, e))
  }

  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}

impl io::Read for PipeRead {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    unistd::read(self.0, buf).map_err(|e| io::Error::new(io::ErrorKind::Other, e))
  }
}

impl Drop for PipeRead {
  fn drop(&mut self) {
    unistd::close(self.0).expect("unable to auto-close read side")
  }
}

impl Drop for PipeWrite {
  fn drop(&mut self) {
    unistd::close(self.0).expect("unable to auto-close pipe")
  }
}
