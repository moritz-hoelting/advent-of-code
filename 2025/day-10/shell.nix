with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    ocaml
    ocamlPackages.ocamlformat
    ocamlPackages.findlib
    ocamlPackages.z3
    dune_3
  ];
}