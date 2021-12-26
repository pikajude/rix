use std::fs::File;
use std::io::Write;
use std::path::Path;

extern crate lalrpop;
extern crate string_cache_codegen;

fn main() {
  println!("cargo:rerun-if-changed=src/syntax/parse.lalrpop");
  lalrpop::process_root().unwrap();

  let mut buf = Vec::<u8>::new();

  string_cache_codegen::AtomType::new("util::ident_gen::Ident", "ident!")
    .atoms(&[
      "null",
      "true",
      "false",
      "__curPos",
      "__findFile",
      "__nixPath",
      "__sub",
      "__mul",
      "__div",
      "or",
      "body",
      "path",
      "prefix",
    ])
    .write_to(&mut buf)
    .unwrap();

  let contents = String::from_utf8_lossy(&buf);
  let path = Path::new(&std::env::var("OUT_DIR").unwrap()).join("ident.rs");
  std::fs::create_dir_all(path.parent().unwrap()).unwrap();
  let mut f = File::create(path).unwrap();
  for ok_parts in contents.split("# [macro_export]") {
    f.write_all(ok_parts.as_bytes()).unwrap();
  }
}
