let
  overlays = [ (import ./overlay.nix) ];
  pkgs = import <nixpkgs> { inherit overlays; };
in pkgs.haskellPackages.taffybar
