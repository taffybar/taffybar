let
  overlays = [ (import ./overlay.nix) ];
  pkgs = (import ./nixpkgs.nix) { inherit overlays; };
in pkgs.haskellPackages.taffybar
