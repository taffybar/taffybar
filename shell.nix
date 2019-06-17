let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs-channels";
    rev    = "7815c86c104a99417db844791dcda34fe7a7965f";
    sha256 = "0k6ws2b2b6vrvq2g5h8fi8qscb0wk0wy097cnf36f9acd126k43j";
  };
  overlays = [ (import ./overlay.nix) ];
  pkgs = import nixpkgs { inherit overlays; };
in pkgs.haskellPackages.shellFor { packages = _: [pkgs.haskellPackages.taffybar]; }
