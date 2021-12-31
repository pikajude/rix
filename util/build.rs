extern crate string_cache_codegen;

use std::error::Error;
use std::path::Path;

use string_cache_codegen::AtomType;

static IDENTS: &[&str] = &[
  "__curPos",
  "__div",
  "__findFile",
  "__functor",
  "__ignoreNulls",
  "__lessThan",
  "__mul",
  "__nixPath",
  "__sub",
  "<none>",
  "body",
  "column",
  "drvPath",
  "file",
  "key",
  "line",
  "name",
  "operator",
  "or",
  "outPath",
  "path",
  "prefix",
  "startSet",
  "success",
  "value",
  "version",
];

fn run() -> Result<(), Box<dyn Error>> {
  let out_file = Path::new(&std::env::var("OUT_DIR")?).join("ident_gen.rs");
  AtomType::new("ident_gen::Ident", "ident!")
    .atoms(IDENTS)
    .with_macro_doc("Macro for interned identifiers")
    .write_to_file(&out_file)?;
  Ok(())
}

fn main() {
  run().unwrap()
}
