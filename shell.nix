let
  pkgs = import ./nix/sources.nix { };
in

with pkgs;

mkShell {
  buildInputs = (with ocamlPackages; [ ocaml dune findlib menhir ]);
}
