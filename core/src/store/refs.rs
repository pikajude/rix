use std::collections::{HashMap, HashSet};
use std::io::{self, Write};
use std::path::Path;

use rix_util::hash::HashResult;

use crate::util::base32::IS_BASE32;

use super::prelude::*;

struct RefScanner {
  hashes: HashSet<Vec<u8>>,
  seen: HashSet<Vec<u8>>,
  hash_sink: HashSink<std::io::Sink>,
  tail: Vec<u8>,
}

impl Default for RefScanner {
  fn default() -> Self {
    Self {
      hashes: Default::default(),
      seen: Default::default(),
      tail: Default::default(),
      hash_sink: HashSink::new(HashType::SHA256, std::io::sink()),
    }
  }
}

const REF_LEN: usize = 32;

impl Write for RefScanner {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.hash_sink.write_all(buf)?;

    self.tail.extend(buf.take(REF_LEN));
    search(&self.tail, &mut self.hashes, &mut self.seen);
    search(buf, &mut self.hashes, &mut self.seen);

    let tail_len = std::cmp::min(buf.len(), REF_LEN);
    let sub_start = self.tail.len().saturating_sub(REF_LEN - tail_len);
    self.tail = self.tail.split_off(sub_start);
    self.tail.extend(&buf[buf.len() - tail_len..]);

    Ok(buf.len())
  }

  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}

fn search(data: &[u8], hashes: &mut HashSet<Vec<u8>>, seen: &mut HashSet<Vec<u8>>) {
  let len = data.len();

  let mut i = 0;
  while i + REF_LEN <= len {
    let mut matched = true;
    let mut j = REF_LEN - 1;
    while j > 0 {
      if !IS_BASE32[data[i + j] as usize] {
        i += j + 1;
        matched = false;
        break;
      }
      j -= 1;
    }
    if !matched {
      continue;
    }
    let maybe_ref = &data[i..i + REF_LEN];
    if hashes.remove(maybe_ref) {
      debug!(
        "found reference to {} at {}",
        String::from_utf8_lossy(maybe_ref),
        i
      );
      seen.insert(maybe_ref.to_vec());
    }
    i += 1;
  }
}

pub fn scan_for_references<'a, P: AsRef<Path>, I: Iterator<Item = &'a StorePath>>(
  path: P,
  refs: I,
) -> Result<(HashResult, Vec<&'a StorePath>)> {
  let mut s = RefScanner::default();
  let mut back_map = HashMap::new();

  for r in refs {
    s.hashes.insert(r.hash_part().into_bytes());
    back_map.insert(r.hash_part().into_bytes(), r);
  }

  crate::util::nar::dump_path(path, &mut s, &PathFilter::none())?;

  let mut found = vec![];
  for path in s.seen {
    found.push(back_map.remove(&path).expect("item missing from back map"));
  }

  Ok((s.hash_sink.finish().1, found))
}
