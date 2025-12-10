with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    swi-prolog
  ];
}