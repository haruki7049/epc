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
          epc = pkgs.stdenv.mkDerivation {
            name = "epc";
            src = lib.cleanSource ./.;

            setupHook = pkgs.writeText "setupHook.sh" ''
              addToSearchPath ERL_LIBS "$1/lib/erlang/lib/"
            '';

            dontStrip = true;
            doCheck = true;

            buildInputs = [
              pkgs.beam28Packages.erlang
            ];

            buildPhase = ''
              runHook preBuild
              make
              runHook postBuild
            '';

            preCheck = ''
              export HOME=$(mktemp -d)
            '';

            checkPhase = ''
              runHook preCheck
              make test
              make dialyzer
              runHook postCheck
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out/lib/erlang/lib/ebin
              cp -r ./ebin/* $out/lib/erlang/lib/ebin
              runHook postInstall
            '';
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
              pkgs.nil # Nix LSP
              pkgs.erlang-language-platform # Erlang LSP
            ];

            buildInputs = [
              pkgs.beam28Packages.hex
            ];

            inputsFrom = [ config.treefmt.build.devShell ];
          };
        };
    };
}
