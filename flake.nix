{
  description = "melange-compiler-libs Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.inputs.flake-utils.follows = "flake-utils";
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs, flake-utils }:
    { overlays.default = import ./nix/overlay.nix; } //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_0;
        });
      in
      rec {
        packages.default = pkgs.callPackage ./nix { };
        devShell = pkgs.callPackage ./nix/shell.nix { };
      });
}
