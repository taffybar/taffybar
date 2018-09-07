# You can build this repository by running:
#   $ nix-build

{
pkgs_path ? ./nix/nixpkgs.nix
}:

let
  overlay = import ./nix/overlay.nix;
  pkgs = import pkgs_path {
    config = {};
    overlays = [ overlay ];
  };
  filter =  import ./nix/filter.nix {inherit (pkgs)lib;};
  haskellPackages = pkgs.haskell.packages.ghc822.override {
    overrides = self: super: {
      taffybar = with pkgs.haskell.lib;
        ((addPkgconfigDepend (disableLibraryProfiling (dontCheck (dontHaddock
          ( pkgs.haskell.packages.ghc822.callCabal2nix
              "taffybar"
              (builtins.path { name = "taffybar"; inherit filter; path = ./.; } )
              { }
          )))) pkgs.gtk3).overrideAttrs (_: { strictDeps = true; }));
        };
  };
  ghcWithTaffybar = haskellPackages.ghcWithPackages (p: with p; [taffybar]);

in

  pkgs.stdenv.mkDerivation {
    name = "taffy-ghc-env";
    nativeBuildInputs = [ pkgs.makeWrapper ];
    buildCommand = ''
      mkdir -p $out/bin
      makeWrapper ${ghcWithTaffybar}/bin/taffybar $out/bin/taffybar \
        --set NIX_GHC "${ghcWithTaffybar}/bin/ghc"
    '';
  }
