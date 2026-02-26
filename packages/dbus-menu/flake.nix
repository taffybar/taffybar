{
  description = "dbus-menu";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix }:
  flake-utils.lib.eachDefaultSystem (system: let
    inherit (nixpkgs) lib;
    pkgs = import nixpkgs {
      inherit system;
      overlays = lib.attrValues self.overlays;
      config.allowBroken = true;
    };
  in
  {
    devShells.default = pkgs.haskellPackages.shellFor {
      packages = p: [ p.dbus-menu ];
      nativeBuildInputs = with pkgs.haskellPackages; [
        cabal-install haskell-language-server
      ];
    };
    packages.default = pkgs.haskellPackages.dbus-menu;
  }) // {
    overlays = {
      default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            dbus-menu =
              hself.callCabal2nix "dbus-menu"
              (git-ignore-nix.lib.gitignoreSource ./.)
              { inherit (final) gtk3; };
          });
        });
      };
    };
  };
}
