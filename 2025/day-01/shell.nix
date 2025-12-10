with import <nixpkgs> {};

mkShell {
  buildInputs = [
    just
    gfortran
    fortran-fpm
  ];
}