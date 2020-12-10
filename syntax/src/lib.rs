#[macro_use] extern crate derive_more;
#[macro_use] extern crate enum_as_inner;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate thiserror;

use anyhow::Result;
use codespan::FileId;
pub use expr::Expr;
use lalrpop_util::ParseError;
use lex::Lexer;
use rix_util::*;
use std::{
  path::Path,
  sync::atomic::{AtomicUsize, Ordering},
};

pub mod expr;
pub mod lex;
pub mod parse;

static INLINE_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn parse_str(file_id: FileId, base_path: &Path, input: &str) -> Result<Expr> {
  parse::ExprParser::new()
    .parse(base_path, file_id, Lexer::new(input, file_id))
    .map_err(|e| LocatedParseError(file_id, e.map_token(|t| t.to_string())).erased())
}

pub fn parse_inline(input: &str) -> Result<Expr> {
  let filename = format!(
    "<inline-{}>",
    INLINE_COUNTER.fetch_add(1, Ordering::Acquire)
  );
  let mut files = FILES.lock();
  let id = files.add(filename, input.into());
  parse_str(
    id,
    &*std::env::current_dir().expect("no current_dir()"),
    files.source(id),
  )
}

pub fn parse_from_file<P: AsRef<Path>>(path: P) -> Result<Expr> {
  let path = path.as_ref();
  let contents = std::fs::read_to_string(path).unwrap();
  let mut files = FILES.lock();
  let id = files.add(path, contents);
  parse_str(id, path.parent().unwrap(), files.source(id))
}

#[derive(Debug, Error)]
pub enum UserError {
  #[error("{0}")]
  Lexer(lex::LexError),
  #[error("{0}")]
  PathResolution(#[from] path_abs::Error),
  #[error("{0}")]
  Other(String),
}

#[derive(Error, Debug)]
#[error("{1:?}")]
struct LocatedParseError(FileId, ParseError<usize, String, Located<UserError>>);

impl LocatedError for LocatedParseError {
  fn diagnose(&self) -> Diagnostic<FileId> {
    let file_id = self.0;
    let (msg, labels) = match self.1 {
      ParseError::InvalidToken { location } => (
        "invalid token".to_string(),
        vec![Label::primary(file_id, location..location)],
      ),
      ParseError::UnrecognizedEOF {
        location,
        ref expected,
      } => (
        "unexpected EOF".to_string(),
        vec![
          Label::primary(file_id, location..location).with_message(format!(
            "expected one of {}",
            itertools::join(expected, ", ")
          )),
        ],
      ),
      ParseError::UnrecognizedToken {
        ref token,
        ref expected,
      } => (
        format!("unexpected token `{}'", token.1),
        vec![
          Label::primary(file_id, token.0..token.2).with_message(format!(
            "expected one of {}",
            itertools::join(expected, ", ")
          )),
        ],
      ),
      ParseError::ExtraToken { ref token } => (
        format!("extra token in input: `{}'", token.1),
        vec![Label::primary(file_id, token.0..token.2)],
      ),
      ParseError::User { ref error } => (
        error.v.to_string(),
        vec![Label::primary(error.pos.0, error.pos.1)],
      ),
    };

    Diagnostic::error().with_labels(labels).with_message(msg)
  }
}
