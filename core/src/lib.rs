#![feature(map_first_last, option_zip, pattern)]

extern crate rix_syntax as syntax;
extern crate rix_util as util;

#[macro_use] extern crate derivative;
#[macro_use] extern crate derive_more;
#[macro_use] extern crate enum_as_inner;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate serde;
#[macro_use] extern crate slog_scope;

pub mod eval;
pub mod fetch;
pub mod store;
