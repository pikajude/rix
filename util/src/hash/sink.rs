use super::{Context, Hash, HashType};
use crate::Result;
use crypto::digest::Digest;
use std::io::{self, Seek, SeekFrom, Write};

pub struct Sink<W> {
  c: Context,
  writer: W,
}

impl<W> Sink<W> {
  pub fn new(ty: HashType, writer: W) -> Self {
    Self {
      c: Context::new(ty),
      writer,
    }
  }

  pub fn finish(mut self) -> (W, Hash, usize) {
    let (hash, len) = self.c.finish();
    (self.writer, hash, len)
  }
}

impl<W: Seek> Sink<W> {
  /// `Sink::finish`, but seeks to the beginning of the stream before returning.
  pub fn finish_reset(mut self) -> Result<(W, Hash, usize)> {
    let (hash, len) = self.c.finish();
    self.writer.seek(SeekFrom::Start(0))?;
    Ok((self.writer, hash, len))
  }
}

impl<W: Write> Write for Sink<W> {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.c.input(buf);
    self.writer.write_all(buf)?;
    Ok(buf.len())
  }

  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}
