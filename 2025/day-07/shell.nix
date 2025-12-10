with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    erlang
    gleam
  ];
}