#![feature(assert_matches, get_mut_unchecked)]

#[macro_use] extern crate enum_as_inner;
#[macro_use] extern crate slog_scope;

pub mod eval;
pub extern crate rix_syntax as syntax;
pub extern crate rix_util as util;
