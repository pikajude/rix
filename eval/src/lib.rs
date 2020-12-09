#[macro_use] extern crate derive_more;
#[macro_use] extern crate enum_as_inner;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate thiserror;

use codespan::{FileId, Files};
use lalrpop_util::ParseError;
use parking_lot::Mutex;
use parse::Located;
use std::sync::atomic::AtomicUsize;

pub mod expr;
pub mod lex;
pub mod parse;

pub type Ident = string_cache::DefaultAtom;

static INLINE_COUNTER: AtomicUsize = AtomicUsize::new(0);

lazy_static! {
  pub(crate) static ref FILES: Mutex<Files<String>> = {
    let mut files = Files::new();
    files.add("<unknown>", "<unknown>".to_string());
    Mutex::new(files)
  };
}

#[derive(Debug, Error)]
pub enum UserError {
  #[error("{0}")]
  Lexer(lex::LexError),
  #[error("{0}")]
  PathResolution(#[from] path_abs::Error),
  #[error("undefined variable {0}")]
  UndefinedVariable(Ident),
  #[error("{0}")]
  Other(String),
}

#[derive(Error, Debug)]
#[error("{0:?}")]
struct LocatedParseError(FileId, ParseError<usize, String, Located<UserError>>);
