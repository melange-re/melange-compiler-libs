{ stdenv, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  pname = "melange-compiler-libs";
  version = "n/a";

  src = ./..;

  duneVersion = "3";
  propagatedBuildInputs = with ocamlPackages; [ menhir menhirLib ];
}
