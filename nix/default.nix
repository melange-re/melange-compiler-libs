{ stdenv, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  pname = "melange-compiler-libs";
  version = "n/a";

  src = ./..;

  duneVersion = "3";
  nativeBuildInputs = [ ocamlPackages.menhir ];
  propagatedBuildInputs = with ocamlPackages; [ menhir menhirLib ];
}
