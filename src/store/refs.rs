use std::collections::{HashMap, HashSet};
use std::io::{self, Write};
use std::path::Path;

use crate::util::base32::IS_BASE32;

use super::prelude::*;

#[derive(Default)]
struct RefScanner {
  hashes: HashSet<Vec<u8>>,
  seen: HashSet<Vec<u8>>,
  tail: Vec<u8>,
}

const REF_LEN: usize = 32;

impl Write for RefScanner {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.tail.extend(slice_take(buf, REF_LEN));
    search(&self.tail, &mut self.hashes, &mut self.seen);
    search(buf, &mut self.hashes, &mut self.seen);

    let tail_len = if buf.len() <= REF_LEN {
      buf.len()
    } else {
      REF_LEN
    };
    let sub_start = if self.tail.len() < REF_LEN - tail_len {
      0
    } else {
      self.tail.len() - (REF_LEN - tail_len)
    };
    self.tail = self.tail.split_off(sub_start);
    self.tail.extend(&buf[buf.len() - tail_len..]);

    Ok(buf.len())
  }

  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}

fn slice_take<T>(s: &[T], take: usize) -> &[T] {
  if take > s.len() {
    s
  } else {
    &s[..take]
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
) -> Result<Vec<&'a StorePath>> {
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

  Ok(found)
}
