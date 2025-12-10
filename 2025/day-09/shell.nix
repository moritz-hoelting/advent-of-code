with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    ghc
    cabal-install
    stylish-haskell
  ];
}