{
  description = ''
    A Nix flake for building and developing Tidal.

    Packages are included for:
    - tidal
    - tidal-link

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
    # Temporarily add `hosc` at the latest v0.20 commit
    # (nixpkgs currently only has 0.19.1).
    # See this comment for details:
    # https://github.com/tidalcycles/Tidal/pull/1022#issuecomment-1610978403
    hosc = {
      url = "github:rd--/hosc?rev=e77aa67cd0b99a32498fef246a687ba443c9b4be";
      flake = false;
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
        hosc = inputs.hosc; # Remove once `hosc` is at 0.20 in nixpkgs.
        tidal = ./.;
        tidal-link = ./tidal-link;
      });
      tidal-boot = ./BootTidal.hs;
      tidal-ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: [project.tidal]);
    in {
      tidal = project.tidal;
      tidal-link = project.tidal-link;
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
