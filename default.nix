{ system ? builtins.currentSystem }:
let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; inherit system; };
in
with nixpkgs;
let platform = rust.makeRustPlatform {
  rustc = latest.rustChannels.nightly.rust;
  cargo = latest.rustChannels.nightly.cargo;
}; in
platform.buildRustPackage rec {
  name = "rnix";
  version = "0.1.0";

  src = builtins.fetchGit ./.;

  cargoSha256 = "18dx0mc1n9234g373s0r5dbdhqvl859k989svi6n2f6c11i1lm7p";

  buildInputs = [ sqlite ];

  PKG_CONFIG_PATH = "${libarchive.dev}/lib/pkgconfig";
  PKG_CONFIG = "${pkgconfig}/bin/pkg-config";
}
