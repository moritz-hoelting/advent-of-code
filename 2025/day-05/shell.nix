with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    rakudo
  ];
}