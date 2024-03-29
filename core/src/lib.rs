#![feature(map_first_last, option_zip, pattern)]

pub extern crate rix_eval as eval;
extern crate rix_syntax as syntax;
extern crate rix_util as util;

pub mod build;
pub mod local_store;
pub mod lock;
pub mod refs;
pub mod settings;
