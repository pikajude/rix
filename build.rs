extern crate lalrpop;

fn main() {
  println!("cargo:rerun-if-changed=src/syntax/parse.lalrpop");
  lalrpop::process_root().unwrap();
}
