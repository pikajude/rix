use std::path::Path;

fn main() {
  string_cache_codegen::AtomType::new("ident_gen::Ident", "ident!")
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
    .write_to_file(&Path::new(&std::env::var("OUT_DIR").unwrap()).join("ident.rs"))
    .unwrap();
}
