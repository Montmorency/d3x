{
  description = "d3x - D3-style SVG visualizations in Haskell with HSX";

  inputs = {
    ihp.url = "github:digitallyinduced/ihp";
    nixpkgs.follows = "ihp/nixpkgs";
  };

  outputs =
    { self, nixpkgs, ihp }:
    let
      allSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems =
        f:
        nixpkgs.lib.genAttrs allSystems (
          system:
          f {
            pkgs = import nixpkgs { inherit system; };
          }
        );
    in
    {
      packages = forAllSystems (
        { pkgs }:
        {
          default = pkgs.haskellPackages.callCabal2nix "d3x" ./. { };
        }
      );

      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = hfinal: hprev: {
            d3x = hfinal.callCabal2nix "d3x" self { };
          };
        };
      };

      devShells = forAllSystems (
        { pkgs }:
        {
          default = pkgs.mkShell {
            packages = with pkgs.haskellPackages; [
              ghc
              cabal-install
            ];
          };
        }
      );
    };
}
