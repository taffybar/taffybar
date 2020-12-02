{
  nixpkgs ? import <nixpkgs>
}:
let
  overlays = [ (import ./overlay.nix) ];
  pkgs = nixpkgs { inherit overlays; };
in pkgs.haskellPackages.taffybar
