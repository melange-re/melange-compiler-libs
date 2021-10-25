{ ocamlVersion ? "4_12" }:
let
  overlays = /Users/anmonteiro/projects/nix-overlays;

in
import "${overlays}/boot.nix" {
  overlays = [
    (import overlays)
    (self: super: {
      ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";
    })
  ];
}
