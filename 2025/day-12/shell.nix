with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    cargo
  ];
}