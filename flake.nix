{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      imports = [
        inputs.treefmt-nix.flakeModule
      ];

      perSystem =
        {
          pkgs,
          lib,
          config,
          ...
        }:
        let
          epc = pkgs.beam28Packages.buildRebar3 {
            name = "epc";
            version = "rolling";
            src = lib.cleanSource ./.;
            beamDeps = [ ];
          };
        in
        {
          treefmt = {
            projectRootFile = ".git/config";

            # Nix
            programs.nixfmt.enable = true;

            # Erlang
            programs.efmt.enable = true;

            # GitHub Actions
            programs.actionlint.enable = true;

            # Json
            programs.jsonfmt.enable = true;

            # Markdown
            programs.mdformat.enable = true;

            # ShellScript
            programs.shellcheck.enable = true;
            programs.shfmt.enable = true;
          };

          packages = {
            inherit epc;
            default = epc;
          };

          checks = {
            inherit epc;
          };

          devShells.default = pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.beam28Packages.erlang # Erlang VM
              pkgs.beam28Packages.rebar3 # Rebar3 build tool

              pkgs.nil # Nix LSP
              pkgs.erlang-language-platform # Erlang LSP
            ];

            inputsFrom = [ config.treefmt.build.devShell ];
          };
        };
    };
}
