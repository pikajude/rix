#![feature(map_first_last, option_zip, pattern)]

extern crate rix_syntax as syntax;
extern crate rix_util as util;

pub mod build;
pub mod eval;
pub mod fetch;
pub mod local_store;
pub mod lock;
pub mod refs;
pub mod settings;
