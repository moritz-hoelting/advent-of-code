with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    python315
  ];
}