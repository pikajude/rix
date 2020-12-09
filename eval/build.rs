extern crate lalrpop;

fn main() {
  println!("cargo:rerun-if-changed=src/parse.lalrpop");
  lalrpop::process_root().unwrap();
}
