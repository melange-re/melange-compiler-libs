{ stdenv, lib, ocamlPackages }:

with ocamlPackages;

buildDunePackage {
  pname = "melange-compiler-libs";
  version = "n/a";

  src = ./..;

  duneVersion = "3";
  nativeBuildInputs = [ menhir ];
  propagatedBuildInputs = [ menhirLib ];
}
