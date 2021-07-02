with import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz") { };
stdenv.mkDerivation {
  name = "xmonad";
  buildInputs = [
    ghc
    zlib
  ];
}
