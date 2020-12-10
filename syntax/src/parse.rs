pub use generated::*;
use std::path::PathBuf;

mod generated {
  #![allow(clippy::all)]
  include!(concat!(env!("OUT_DIR"), "/parse.rs"));
}

fn homedir() -> PathBuf {
  std::env::var("HOME")
    .expect("variable $HOME not set")
    .into()
}
