use super::{Context, Hash, HashType};
use crypto::digest::Digest;
use std::io::{self, Write};

pub struct Sink(Context);

impl Sink {
  pub fn new(ty: HashType) -> Self {
    Self(Context::new(ty))
  }

  pub fn finish(&mut self) -> (Hash, usize) {
    self.0.finish()
  }
}

impl Write for Sink {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.0.input(buf);
    Ok(buf.len())
  }

  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}
