{
  description = "gtk-strut";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix.url = "github:hercules-ci/gitignore.nix";
    git-ignore-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix }:
  flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ self.overlays.default ];
      config.allowBroken = true;
    };
  in {
    devShells.default = pkgs.haskellPackages.shellFor {
      packages = p: [ p.gtk-strut ];
      nativeBuildInputs = with pkgs.haskellPackages; [
        cabal-install hlint ghcid ormolu implicit-hie haskell-language-server
      ];
    };
    packages.default = pkgs.haskellPackages.gtk-strut;
  }) // {
    overlays.default = final: prev: {
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
  };
}
