let
  overlays = [ (import ./overlay.nix) ];
  pkgs = (import ./nixpkgs.nix) { inherit overlays; };
in pkgs.haskellPackages.shellFor { packages = p: [ p.taffybar ]; }
