let config = {
  packageOverrides = import ./overlay.nix;
  };
  pkgs = import <nixpkgs> { inherit config; };
in pkgs.haskellPackages.taffybar
