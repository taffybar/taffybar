{ nixpkgs ? (import <nixpkgs>) }:
let pkgs = nixpkgs {
  overlays = [ (import ./overlay.nix) ];
};
in pkgs.haskellPackages.xdg-desktop-entry
