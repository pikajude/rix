#![feature(assert_matches, get_mut_unchecked)]

#[macro_use] extern crate derive_more;
#[macro_use] extern crate enum_as_inner;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate thiserror;
#[macro_use] extern crate slog_scope;

pub mod eval;
pub mod syntax;
pub extern crate rix_util as util;
