with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "tldr-hs";
  buildInputs = [
    ghc
    zlib
  ];
}
