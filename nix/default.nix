{ stdenv, lib, ocamlPackages }:

with ocamlPackages;

buildDunePackage {
  pname = "melange-compiler-libs";
  version = "n/a";

  src = ./..;

  useDune2 = true;

  nativeBuildInputs = [ ocaml dune findlib ];
  propagatedBuildInputs = [ menhir menhirLib ];
}
