with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    fpc
  ];
}