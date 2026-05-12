d3x
===

Inspired by the results of [d3.js](https://d3js.org/).

d3x combines a type class for defining common mapping [scales](https://medium.com/@mbostock/introducing-d3-scale-61980c51545f)
with `ihp-hsx` quasiquotes to enable type-safe embedding of charts and
geographic visualizations directly in your Haskell web applications.

The goal is a step-by-step port of the core, business-essential charts and
diagrams that d3.js made ubiquitous — bar charts, line charts, choropleths,
and the projection/streaming geo pipeline — expressed natively as SVG inside
HSX so that data and markup share the same type system.


Integrating with an IHP flake.nix
---------------------------------

d3x is designed to drop into an existing IHP project. It is not on Hackage,
so it's pulled in as a flake input and wired through `callCabal2nix`.

```nix
{
  inputs = {
    ihp.url = "github:digitallyinduced/ihp";
    nixpkgs.follows = "ihp/nixpkgs";
    flake-parts.follows = "ihp/flake-parts";
    devenv.follows = "ihp/devenv";
    systems.follows = "ihp/systems";

    # Add d3x as a flake input (not on Hackage)
    d3x.url = "github:Montmorency/d3x";
  };

  # Thread d3x through to perSystem
  outputs = inputs@{ self, nixpkgs, ihp, flake-parts, systems, d3x, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import systems;
      imports = [ ihp.flakeModules.default ];

      perSystem = { pkgs, system, config, ... }: {
        ihp = {
          enable = true;
          appName = "yourApp";
          projectPath = ./.;

          haskellPackages = p: with p; [
            p.ihp
            cabal-install
            base
            wai
            text
            # ... your other deps ...

            # Build d3x from the flake input
            (pkgs.haskell.lib.disableLibraryProfiling (p.callCabal2nix "d3x" d3x {}))
          ];
        };
      };
    };
}
```

And start plotting type-safe charts.
