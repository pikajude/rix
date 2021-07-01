with import <nixpkgs> {};

pkgs.mkShell {
  buildInputs = with pkgs; [
    rustup
    openssl
    curl
    pkgconfig
    libarchive
    sqlite
  ];
}
