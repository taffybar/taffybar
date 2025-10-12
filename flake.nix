{
  description = "gtk-strut";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix.url = "github:hercules-ci/gitignore.nix";
    git-ignore-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix }:
  let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
          gtk-strut =
            hself.callCabal2nix "gtk-strut"
            (git-ignore-nix.lib.gitignoreSource ./.)
            { };
        });
      });
    };
    overlays = [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.gtk-strut ];
      nativeBuildInputs = with pkgs.haskellPackages; [
        cabal-install hlint ghcid ormolu implicit-hie haskell-language-server
      ];
    };
    defaultPackage = pkgs.haskellPackages.gtk-strut;
  }) // { inherit overlay overlays; } ;
}
