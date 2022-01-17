use std::path::Path;
use std::process::Command;

fn main() {
  let out_path = Path::new(&std::env::var("OUT_DIR").unwrap()).join("bootstrap-shell");
  if !Command::new("nix-build")
    .args(["<nixpkgs>", "-A", "pkgsStatic.bash", "--out-link"])
    .arg(&out_path)
    .status()
    .unwrap()
    .success()
  {
    eprintln!("unable to build static bash");
    std::process::exit(1);
  }
  let realpath = std::fs::canonicalize(out_path)
    .expect("out_path doesn't exist")
    .join("bin/bash");
  println!("cargo:rustc-env=BOOTSTRAP_SHELL={}", realpath.display());
}
