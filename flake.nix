{
  description = "melange-compiler-libs Nix Flake";
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs }:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      nixpkgsFor = forAllSystems (system:
        nixpkgs.legacyPackages.${system}.extend (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_3;
        }));
    in

    {
      overlays.default = import ./nix/overlay.nix;
      packages =
        (forAllSystems (system:
          let
            pkgs = nixpkgsFor.${system};
          in
          {
            default = pkgs.callPackage ./nix { };
          }));

      devShells = forAllSystems (system: {
        default = nixpkgsFor.${system}.callPackage ./nix/shell.nix { };
      });
    };
}
