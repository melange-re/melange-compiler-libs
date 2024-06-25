{ packages
, mkShell
, stdenv
, lib
, cacert
, curl
, ocamlPackages
, git
, h2spec
, release-mode ? false
}:

mkShell {
  inputsFrom = lib.attrValues packages;
  buildInputs = (if release-mode then [
    cacert
    curl
    ocamlPackages.dune-release
    git
  ] else [ ]) ++ (with ocamlPackages; [ merlin ocamlformat utop ]);
}
