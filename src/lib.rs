#![feature(btree_drain_filter)]
#![feature(command_access)]
#![feature(crate_visibility_modifier)]
#![feature(doc_cfg)]
#![feature(duration_zero)]
#![feature(fn_traits)]
#![feature(pattern)]
#![feature(or_patterns)]
#![feature(seek_convenience)]
#![feature(try_blocks)]
#![feature(type_name_of_val)]
#![feature(unboxed_closures)]
#![feature(untagged_unions)]
#![deny(clippy::all)]

#[macro_use(trace, debug, info, warn, error)] extern crate slog_scope;
#[macro_use] extern crate derive_more;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate gen_settings;

pub mod archive;
pub mod arena;
pub mod build;
pub mod derivation;
pub mod eval;
pub mod fetch;
pub mod globals;
pub mod hash;
pub mod logger;
pub mod path;
pub mod path_info;
mod prelude;
pub mod settings;
pub mod sqlite;
pub mod store;
pub mod sync;
pub mod syntax;
pub mod util;

pub use settings::Settings;
pub use store::Store;

pub fn settings() -> &'static Settings {
  Settings::get()
}
