{
  description = ''
    A Nix flake for building and developing Tidal.

    Packages are included for:
    - tidal
    - tidal-core
    - tidal-link
    - tidal-listener
    - tidal-parse

    A `tidal-ghci` package is also included. This is a small script that starts
    an instance of `GHCi` with `Tidal` installed and with the `BootTidal.hs`
    file passed as the `-ghci-script`.

    Packages can be built with `nix build .#tidal` or ran with `nix run
    .#tidal-ghci`.

    A `devShell` is included that provides `cabal-install`, `stack` and all
    other build inputs for the tidal packages above included under a temporary
    shell. This shell can be entered with `nix develop`.
  '';

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # Manually include `hosc` at the `v0.21.0`
    hosc = {
      flake = false;
      url = "github:rd--/hosc?rev=43bb2d07ff8d65cf9e51d1f5f96d0e6ffd6fe8fa";
    };
  };

  outputs = inputs: let
    utils.supportedSystems = [
      "aarch64-linux"
      "i686-linux"
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ];
    utils.eachSupportedSystem =
      inputs.utils.lib.eachSystem utils.supportedSystems;

    mkPackages = pkgs: let
      project = pkgs.haskellPackages.extend (pkgs.haskell.lib.compose.packageSourceOverrides {
        hosc = inputs.hosc; # Manually added as `hosc` 0.21 is not yet in nixpkgs.
        tidal = ./.;
        tidal-core = ./tidal-core;
        tidal-link = ./tidal-link;
        tidal-listener = ./tidal-listener;
        tidal-parse = ./tidal-parse;
      });
      tidal-boot = ./BootTidal.hs;
      tidal-ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: [project.tidal]);
    in {
      tidal = project.tidal;
      tidal-core = project.tidal-core;
      tidal-link = project.tidal-link;
      tidal-listener = project.tidal-listener;
      tidal-parse = project.tidal-parse;
      tidal-ghci = pkgs.writeShellScriptBin "tidal-ghci" ''
        ${tidal-ghc}/bin/ghci -ghci-script ${tidal-boot}
      '';
      default = inputs.self.packages.${pkgs.system}.tidal-ghci;
    };

    mkDevShells = pkgs: tidalpkgs: {
      tidal = pkgs.mkShell {
        inputsFrom = pkgs.lib.attrValues tidalpkgs;
        buildInputs = [
          pkgs.cabal-install
          pkgs.stack
        ];
      };
      default = inputs.self.devShells.${pkgs.system}.tidal;
    };

    mkOutput = system: let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
    in {
      packages = mkPackages pkgs;
      devShells = mkDevShells pkgs inputs.self.packages.${system};
      formatter = pkgs.alejandra;
    };

    systemOutputs = utils.eachSupportedSystem mkOutput;
  in
    systemOutputs;
}
