# You can build this repository by running:
#   $ nix-build

{
  pkgs_path ? ./nix/nixpkgs.nix
, compiler ? "ghc822"
}:

let
  overlay = import ./nix/overlay.nix;
  pkgs = import pkgs_path {
    config = {};
    overlays = [ overlay ];
  };
  filter =  import ./nix/filter.nix {inherit (pkgs)lib;};
  hpkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      taffybar = with pkgs.haskell.lib;
        (addPkgconfigDepend (disableLibraryProfiling (dontCheck (dontHaddock
          ( pkgs.haskell.packages.${compiler}.callCabal2nix
              "taffybar"
              (builtins.path { name = "taffybar"; inherit filter; path = ./.; } )
              { }
          )))) pkgs.gtk3);
        };
  };

in
  pkgs.taffybar.override {
    inherit (hpkgs) ghcWithPackages;
  }
