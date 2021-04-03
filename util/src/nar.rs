use crate::*;
use nix::{
  fcntl,
  libc::{S_IXGRP, S_IXOTH, S_IXUSR},
};
use std::{
  collections::BTreeMap,
  fs::{self, File},
  io::{self, Read, Write},
  os::unix::{
    fs::{symlink, MetadataExt, PermissionsExt},
    io::AsRawFd,
  },
};

const VERSION_MAGIC: &str = "nix-archive-1";

pub trait PathFilterFn = Fn(&Path) -> Result<bool>;

pub struct PathFilter(Box<dyn PathFilterFn + Sync>);

impl PathFilter {
  pub fn new<F: PathFilterFn + Sync + 'static>(f: F) -> Self {
    Self(Box::new(f))
  }

  pub fn none() -> Self {
    Self(Box::new(|_| Ok(true)))
  }
}

struct Source<R> {
  reader: R,
}

impl<R: Read> Source<R> {
  fn read_tag_sized(&mut self, max_len: usize) -> Result<Vec<u8>> {
    let len = self.read_usize()?;
    if len > max_len {
      bail!(
        "input string exceeds specified max {} (actual length: {})",
        max_len,
        len
      );
    }
    let mut data = vec![0u8; len];
    self.reader.read_exact(&mut data)?;
    self.read_padding(len)?;
    Ok(data)
  }

  fn read_tag(&mut self) -> Result<Vec<u8>> {
    self.read_tag_sized(std::usize::MAX)
  }

  fn read_usize(&mut self) -> Result<usize> {
    let mut buf = [0u8; 8];
    self.reader.read_exact(&mut buf)?;

    let n = buf[0] as usize
      | (buf[1] as usize) << 8
      | (buf[2] as usize) << 16
      | (buf[3] as usize) << 24
      | (buf[4] as usize) << 32
      | (buf[5] as usize) << 40
      | (buf[6] as usize) << 48
      | (buf[7] as usize) << 56;

    Ok(n)
  }

  fn read_padding(&mut self, len: usize) -> Result<()> {
    if len % 8 > 0 {
      let mut padding = vec![0u8; 8 - (len % 8)];
      self.reader.read_exact(&mut padding)?;
      if padding.iter().any(|x| *x != 0) {
        bail!("non-zero padding")
      }
    }
    Ok(())
  }
}

struct Sink<W> {
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

  pub fn receive<R: Read>(&mut self, len: usize, reader: &mut R) -> Result<()> {
    self.write_usize(len)?;
    io::copy(&mut reader.take(len as u64), &mut self.writer)?;
    self.pad(len)
  }

  // pub fn strings<'a>(&mut self, tags: impl ExactSizeIterator<Item = &'a str>)
  // -> Result<()> {   self.write_usize(tags.len())?;
  //   self.tags(tags)
  // }

  fn write_usize(&mut self, size: usize) -> Result<()> {
    let mut buf = [0u8; 8];
    buf[0] = size as u8;
    buf[1] = (size >> 8) as u8;
    buf[2] = (size >> 16) as u8;
    buf[3] = (size >> 24) as u8;
    buf[4] = (size >> 32) as u8;
    buf[5] = (size >> 40) as u8;
    buf[6] = (size >> 48) as u8;
    buf[7] = (size >> 56) as u8;
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

fn dump_file<W: Write, P: AsRef<Path>>(path: P, len: usize, sink: &mut Sink<W>) -> Result<()> {
  sink.tag("contents")?;
  sink.receive(len, &mut File::open(path)?)
}

pub fn dump_with_len<R: Read, W: Write>(len: usize, mut source: R, sink: W) -> Result<()> {
  let mut sink = Sink { writer: sink };
  sink.tags(vec![VERSION_MAGIC, "(", "type", "regular", "contents"])?;
  sink.receive(len, &mut source)?;
  sink.tag(")")
}

pub fn restore_path<P: AsRef<Path>, R: Read>(path: P, source: R) -> Result<()> {
  let mut src = Source { reader: source };
  let nar_vers = src.read_tag_sized(VERSION_MAGIC.len())?;
  if nar_vers != VERSION_MAGIC.as_bytes() {
    bail!("input is not a Nix archive");
  }

  do_restore(&mut src, path.as_ref())
}

fn do_restore<R: Read>(src: &mut Source<R>, path: &Path) -> Result<()> {
  #[derive(Eq, PartialEq, Debug)]
  enum PathType {
    Unknown,
    File,
    Dir,
    Link,
  }

  let mut cur_type = PathType::Unknown;
  let mut cur_file = None;

  ensure!(src.read_tag()? == b"(", "bad open tag");

  loop {
    let tag = src.read_tag()?;

    match &tag[..] {
      b")" => break,
      b"type" => {
        if cur_type != PathType::Unknown {
          bail!("multiple type fields");
        }
        match src.read_tag()?.as_slice() {
          b"regular" => {
            cur_type = PathType::File;
            cur_file = Some(File::create(path)?);
          }
          b"directory" => {
            cur_type = PathType::Dir;
            fs::create_dir(path)?;
          }
          b"symlink" => {
            cur_type = PathType::Link;
          }
          x => bail!("unrecognized entry type {:?}", String::from_utf8_lossy(x)),
        }
      }
      b"contents" if cur_type == PathType::File => {
        let left = src.read_usize()?;

        let file_sink = cur_file
          .as_mut()
          .ok_or_else(|| anyhow!("error: no current-file set"))?;

        fcntl::posix_fallocate(file_sink.as_raw_fd(), 0, left as i64)?;

        let mut limited = (&mut src.reader).take(left as u64);

        io::copy(&mut limited, file_sink)?;
        file_sink.flush()?;

        src.read_padding(left)?;
      }
      b"executable" if cur_type == PathType::File => {
        ensure!(
          src.read_tag()?.is_empty(),
          "executable marker should be empty"
        );

        let mut perms = fs::metadata(path)?.permissions();
        let old_mode = perms.mode();
        perms.set_mode(old_mode | S_IXUSR | S_IXGRP | S_IXOTH);
        fs::set_permissions(path, perms)?;
      }
      b"entry" if cur_type == PathType::Dir => {
        let mut name = String::new();
        let mut prev_name = String::new();

        ensure!(src.read_tag()? == b"(", "expected open tag");

        loop {
          match src.read_tag()?.as_slice() {
            b")" => break,
            b"name" => {
              name = String::from_utf8_lossy(&src.read_tag()?).to_string();
              if name.is_empty()
                || name == "."
                || name == ".."
                || name.contains('/')
                || name.contains('\x00')
              {
                bail!("NAR contains invalid file name `{:?}'", name);
              }
              if name <= prev_name {
                bail!("NAR is not in order");
              }
              prev_name = name.clone();
            }
            b"node" => {
              if name.is_empty() {
                bail!("entry name missing");
              }
              do_restore(src, &path.join(&name))?;
            }
            x => bail!("unknown field {:?}", String::from_utf8_lossy(x)),
          }
        }
      }
      b"target" if cur_type == PathType::Link => {
        let target = src.read_tag()?;
        symlink(path, &*String::from_utf8_lossy(&target))?;
      }
      x => bail!("unknown field {:?}", String::from_utf8_lossy(x)),
    }
  }

  Ok(())
}

pub fn dump_path<P: AsRef<Path>, W: Write>(path: P, sink: W, filter: &PathFilter) -> Result<()> {
  let mut sink = Sink { writer: sink };
  sink.tag(VERSION_MAGIC)?;

  do_dump(&mut sink, path.as_ref(), filter)
}

fn do_dump<W: Write>(sink: &mut Sink<W>, path: &Path, filter: &PathFilter) -> Result<()> {
  let meta = fs::symlink_metadata(path)?;
  let ty = meta.file_type();

  sink.tag("(")?;

  if ty.is_file() {
    sink.tags(vec!["type", "regular"])?;
    if meta.mode() & S_IXUSR != 0 {
      sink.tags(vec!["executable", ""])?;
    }
    dump_file(path, meta.len() as usize, sink)?;
  } else if ty.is_dir() {
    sink.tags(vec!["type", "directory"])?;
    let mut ordered_files = BTreeMap::new();
    for file in fs::read_dir(path)? {
      let file = file?;
      ordered_files.insert(
        file.file_name().to_string_lossy().to_string(),
        file.file_name().to_string_lossy().to_string(),
      );
    }

    for (hacked_name, real_name) in ordered_files {
      if (filter.0)(&path.join(&hacked_name))? {
        sink.tags(vec!["entry", "(", "name", &*hacked_name, "node"])?;
        do_dump(sink, &path.join(real_name), filter)?;
        sink.tag(")")?;
      }
    }
  } else if ty.is_symlink() {
    let real_path = fs::canonicalize(path)?;
    sink.tags(vec![
      "type",
      "symlink",
      "target",
      real_path.display().to_string().as_str(),
    ])?;
  } else {
    bail!("file `{}' has an unsupported type", path.display());
  }

  sink.tag(")")
}
