#![feature(assert_matches)]
#![feature(get_mut_unchecked)]
#![feature(pattern)]
#![feature(termination_trait_lib)]
#![feature(trait_alias)]
#![feature(try_trait_v2)]

#[macro_use] extern crate derive_more;
#[macro_use] extern crate enum_as_inner;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate serde;
#[macro_use] extern crate thiserror;
#[macro_use] extern crate slog_scope;

pub mod eval;
pub mod syntax;
pub mod util;
