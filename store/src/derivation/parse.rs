use super::Derivation;
use crate::{DerivationType, FileIngestionMethod, HashType, Output, Store};
use rix_util::*;
use std::collections::{BTreeSet, HashSet};
use std::fmt::Display;
use std::path::Path;
use std::str::pattern::Pattern;

use super::FixedOutputHash;

impl Derivation {
  pub fn parse<S: Store + ?Sized, C: AsRef<str>>(
    store: &S,
    name: String,
    contents: C,
  ) -> anyhow::Result<Self> {
    let contents = contents.as_ref();
    let mut drv = Self {
      name,
      ..Default::default()
    };
    let mut parser = Parser::new(contents);

    parser.expect("Derive([")?;

    while !parser.is_end_of_list() {
      parser.expect('(')?;
      let id = parser.string()?;
      parser.expect(',')?;
      let path_str = parser.string()?;
      parser.expect(',')?;
      let hash_algo = parser.string()?;
      parser.expect(',')?;
      let hash = parser.string()?;
      parser.expect(')')?;

      drv
        .outputs
        .insert(id, parse_output(store, path_str, hash_algo, hash)?);
    }

    parser.expect(",[")?;

    while !parser.is_end_of_list() {
      parser.expect('(')?;
      let drv_path = parser.string()?;
      parser.expect(",[")?;
      drv.input_derivations.insert(
        store.parse_store_path(Path::new(&drv_path))?,
        parser.strings()?,
      );
      parser.expect(')')?;
    }

    parser.expect(",[")?;
    let sources = parser
      .paths()?
      .into_iter()
      .map(|x| store.parse_store_path(Path::new(&x)))
      .collect::<anyhow::Result<BTreeSet<_>>>()?;
    drv.input_sources = sources;

    parser.expect(',')?;
    drv.platform = parser.string()?;

    parser.expect(',')?;
    drv.builder = parser.string()?.into();

    parser.expect(",[")?;
    while !parser.is_end_of_list() {
      drv.args.push(parser.string()?);
    }

    parser.expect(",[")?;
    while !parser.is_end_of_list() {
      parser.expect('(')?;
      let name = parser.string()?;
      parser.expect(',')?;
      let value = parser.string()?;
      parser.expect(')')?;
      drv.env.insert(name, value);
    }

    parser.expect(')')?;

    drv.ty = drv.determine_type()?;
    Ok(drv)
  }

  fn determine_type(&self) -> anyhow::Result<DerivationType> {
    let mut ia = HashSet::new();
    let mut fixed = HashSet::new();
    let mut floating = HashSet::new();
    let mut deferred = HashSet::new();
    let mut floating_type = None;
    for (name, out) in &self.outputs {
      match out {
        Output::InputAddressed(_) => {
          ia.insert(name);
        }
        Output::Fixed(_) => {
          fixed.insert(name);
        }
        Output::Floating(_, hashtype) => {
          floating.insert(name);
          match floating_type {
            None => {
              floating_type = Some(*hashtype);
            }
            Some(t) => ensure!(
              t == *hashtype,
              "all floating outputs must use the same type"
            ),
          }
        }
        Output::Deferred => {
          deferred.insert(name);
        }
      }
    }

    match (ia.len(), fixed.len(), floating.len(), deferred.len()) {
      (0, 0, 0, 0) => bail!("must have at least one output"),
      (_, 0, 0, 0) => Ok(DerivationType::InputAddressed),
      (0, x, 0, 0) => {
        ensure!(x == 1, "only one fixed output is allowed");
        ensure!(
          *fixed.iter().next().unwrap() == "out",
          "fixed output must be named \"out\""
        );
        Ok(DerivationType::Fixed)
      }
      (0, 0, _, 0) => Ok(DerivationType::Floating),
      (0, 0, 0, _) => Ok(DerivationType::DeferredInputAddressed),
      _ => bail!("can't mix output types in one derivation"),
    }
  }
}

fn parse_output<S: Store + ?Sized>(
  store: &S,
  path: String,
  hash_algo: String,
  hash: String,
) -> anyhow::Result<Output> {
  if !hash_algo.is_empty() {
    let (method, algo) = if let Some(h) = hash_algo.strip_prefix("r:") {
      (FileIngestionMethod::Recursive, h)
    } else {
      (FileIngestionMethod::Flat, &*hash_algo)
    };
    let hash_type = algo.parse::<HashType>()?;
    if !hash.is_empty() {
      ensure!(
        Path::new(&path).starts_with("/"),
        "path `{}' is invalid",
        path
      );
      Ok(Output::Fixed(FixedOutputHash {
        method,
        hash: Hash::decode_with_type(&hash, hash_type, false)?,
      }))
    } else {
      ensure!(
        path.is_empty(),
        "floating content-addressed path must be empty"
      );
      Ok(Output::Floating(method, hash_type))
    }
  } else if path.is_empty() {
    Ok(Output::Deferred)
  } else {
    ensure!(
      Path::new(&path).starts_with("/"),
      "path `{}' is invalid",
      path
    );
    Ok(Output::InputAddressed(
      store.parse_store_path(Path::new(&path))?,
    ))
  }
}

struct Parser<'input> {
  #[deprecated = "use the input() method"]
  _input: &'input str,
  pos: usize,
}

type Result<T> = std::result::Result<T, Error>;

impl<'input> Parser<'input> {
  fn new(input: &'input str) -> Self {
    #[allow(deprecated)]
    Self {
      _input: input,
      pos: 0,
    }
  }

  fn input(&self) -> &'input str {
    #[allow(deprecated)]
    &self._input[self.pos..]
  }

  fn expect<P: Pattern<'input> + Display + Copy>(&mut self, pat: P) -> Result<&'input str> {
    if let Some(inp) = self.input().matches(pat).next() {
      self.pos += inp.len();
      Ok(inp)
    } else {
      self.err(ErrorKind::Expected(pat.to_string()))
    }
  }

  fn peek(&mut self) -> Option<char> {
    self.input().chars().next()
  }

  fn next(&mut self) -> Result<char> {
    match self.peek() {
      Some(x) => {
        self.pos += x.len_utf8();
        Ok(x)
      }
      None => self.err(ErrorKind::Expected("any character".into())),
    }
  }

  fn string(&mut self) -> Result<String> {
    self.expect('"')?;
    let mut ch = self.next()?;
    let mut buf = String::new();
    while ch != '"' {
      if ch == '\\' {
        match self.next()? {
          'n' => buf.push('\n'),
          'r' => buf.push('\r'),
          't' => buf.push('\t'),
          x => buf.push(x),
        }
      } else {
        buf.push(ch)
      }
      ch = self.next()?;
    }
    Ok(buf)
  }

  fn path(&mut self) -> Result<String> {
    let s = self.string()?;
    if !s.starts_with('/') {
      return self.err(ErrorKind::InvalidPath);
    }
    Ok(s)
  }

  fn strings(&mut self) -> Result<BTreeSet<String>> {
    let mut res = BTreeSet::new();
    while !self.is_end_of_list() {
      res.insert(self.string()?);
    }
    Ok(res)
  }

  fn paths(&mut self) -> Result<BTreeSet<String>> {
    let mut res = BTreeSet::new();
    while !self.is_end_of_list() {
      res.insert(self.path()?);
    }
    Ok(res)
  }

  fn is_end_of_list(&mut self) -> bool {
    match self.peek() {
      Some(',') => {
        self.pos += 1;
        false
      }
      Some(']') => {
        self.pos += 1;
        true
      }
      _ => false,
    }
  }

  fn err<T>(&self, k: ErrorKind) -> Result<T> {
    Err(Error {
      input: self.input().to_string(),
      kind: k,
    })
  }
}

#[derive(Debug, Display, Error)]
#[display(fmt = "{}, at {:?}", kind, input)]
struct Error {
  input: String,
  kind: ErrorKind,
}

#[derive(Debug, Display)]
enum ErrorKind {
  #[display(fmt = "expected `{}'", _0)]
  Expected(String),
  #[display(fmt = "path must start with a `/'")]
  InvalidPath,
}
