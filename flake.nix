{
  description = "A very basic flake";

  inputs = {
    # House keeping
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    # Haskell bits
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    MHaP = {
      url = "github:mlabs-haskell/mlabs-haskell-packages?ref=gh-pages";
      flake = false;
    };

    cardano-node.url = "github:intersectmbo/cardano-node/10.1.3";
  };

  outputs = inputs@{ flake-parts, nixpkgs, haskell-nix, iohk-nix, CHaP, MHaP, pre-commit-hooks, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.hercules-ci-effects.flakeModule
        inputs.pre-commit-hooks.flakeModule
      ];

      systems = [ "x86_64-linux" ];
      hercules-ci.github-pages.branch = "master";

      perSystem = { config, system, self', ... }:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          haskell-pkgs =
            import haskell-nix.inputs.nixpkgs {
              inherit system;
              overlays = [
                haskell-nix.overlay
                iohk-nix.overlays.crypto
                iohk-nix.overlays.haskell-nix-crypto
                (_: super: {
                  haskell-nix = super.haskell-nix // {
                    extraPkgconfigMappings = super.haskell-nix.extraPkgconfigMappings // {
                      "PostgresSQL" = [ "postgresql" ];
                      "libpq" = [ "postgresql" ];
                      "pq" = [ "postgresql" ];
                    };
                  };
                })
              ];
              inherit (haskell-nix) config;
            };
          project = haskell-pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc966";
            index-state = "2025-01-10T00:00:00Z";
            inputMap = {
              "https://chap.intersectmbo.org/" = CHaP;
              "https://mlabs-haskell.github.io/mlabs-haskell-packages" = MHaP;
            };
            shell = {
              withHoogle = true;
              withHaddock = true;
              exactDeps = false;
              buildInputs = [
                haskell-pkgs.postgresql
                inputs.cardano-node.packages.${system}.cardano-node
                inputs.cardano-node.packages.${system}.cardano-cli
              ];
              tools = {
                cabal = { };
                haskell-language-server = { };
                hlint = { };
                apply-refact = { };
                cabal-fmt = { };
                ormolu = { };
                hspec-discover = { };
                markdown-unlit = { };
              };
              shellHook = ''
                ${config.pre-commit.installationScript}
              '';
            };
          };
          flake = project.flake { };
        in
        {
          inherit (flake) devShells;
          packages = flake.packages // {

          };
          checks = flake.checks // {
            on-chain-tests = pkgs.stdenv.mkDerivation
              {
                name = "block-buster-test";
                src = ./.;
                nativeBuildInputs = [
                  flake.packages."block-buster:exe:block-buster-test"
                ];
                buildPhase = ''
                  block-buster-test
                  touch $out
                '';
              };

          };
          hercules-ci.github-pages.settings.contents = self'.packages.documents;

          pre-commit = {
            settings = {
              src = ./.;

              hooks = {
                nixpkgs-fmt.enable = true;
                statix.enable = true;
                deadnix.enable = true;

                cabal-fmt.enable = true;
                fourmolu = {
                  enable = true;
                  excludes = [ "\.lhs" ];
                };
                ormolu = {
                  settings.cabalDefaultExtensions = true;
                };
                hlint.enable = true;

                typos = {
                  enable = true;
                  excludes = [ "\.golden" "fourmolu.yaml" ];
                };

                yamllint.enable = true;
              };
            };
          };
        };
    };
}
