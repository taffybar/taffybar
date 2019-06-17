let
  overlays = [ (import ./overlay.nix) ];
  pkgs = (import ./nixpkgs.nix) { inherit overlays; };
in pkgs.haskellPackages.shellFor { packages = _: [pkgs.haskellPackages.taffybar]; }
