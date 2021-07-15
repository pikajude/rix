#![feature(assert_matches)]
#![feature(backtrace)]
#![feature(backtrace_frames)]
#![feature(pattern)]
#![feature(io_read_to_string)]
#![feature(termination_trait_lib)]
#![feature(trait_alias)]
#![feature(try_blocks)]
#![feature(try_trait)]
#![feature(option_zip)]
#![feature(map_first_last)]

#[macro_use] extern crate derivative;
#[macro_use] extern crate derive_more;
#[macro_use] extern crate enum_as_inner;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate serde;
#[macro_use] extern crate thiserror;
#[macro_use] extern crate slog_scope;

pub mod eval;
pub mod fetch;
pub mod store;
pub mod syntax;
pub mod util;
