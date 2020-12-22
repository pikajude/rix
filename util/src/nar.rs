use crate::*;
use std::io::{Read, Write};

const VERSION_MAGIC: &str = "nix-archive-1";

pub struct Sink<W> {
  writer: W,
}

impl<W: Write> Sink<W> {
  pub fn tag(&mut self, tag: &str) -> Result<()> {
    self.write_usize(tag.len())?;
    self.writer.write_all(tag.as_bytes())?;
    self.pad(tag.len())
  }

  pub fn tags<'a>(&mut self, tags: impl IntoIterator<Item = &'a str>) -> Result<()> {
    for t in tags {
      self.tag(t)?;
    }
    Ok(())
  }

  pub fn strings<'a>(&mut self, tags: impl ExactSizeIterator<Item = &'a str>) -> Result<()> {
    self.write_usize(tags.len())?;
    self.tags(tags)
  }

  fn write_usize(&mut self, len: usize) -> Result<()> {
    let mut buf = [0u8; 8];
    buf[0] = len as u8;
    buf[1] = (len >> 8) as u8;
    buf[2] = (len >> 16) as u8;
    buf[3] = (len >> 24) as u8;
    buf[4] = (len >> 32) as u8;
    buf[5] = (len >> 40) as u8;
    buf[6] = (len >> 48) as u8;
    buf[7] = (len >> 56) as u8;
    self.writer.write_all(&buf)?;
    Ok(())
  }

  fn pad(&mut self, len: usize) -> Result<()> {
    if len % 8 > 0 {
      let zeroes = vec![0u8; 8 - (len % 8)];
      self.writer.write_all(&zeroes)?;
    }
    Ok(())
  }
}

pub fn dump_string<W: Write>(source: &str, sink: W) -> Result<()> {
  dump_with_len(source.len(), source.as_bytes(), sink)
}

pub fn dump_with_len<R: Read, W: Write>(len: usize, mut source: R, sink: W) -> Result<()> {
  let mut sink = Sink { writer: sink };
  sink.tags(vec![VERSION_MAGIC, "(", "type", "regular", "contents"])?;
  sink.write_usize(len)?;
  std::io::copy(&mut source, &mut sink.writer)?;
  sink.pad(len)?;
  sink.tag(")")
}
