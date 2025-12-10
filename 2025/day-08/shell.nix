with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    deno
  ];
}