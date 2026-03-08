{
  description = "gtk-scaling-image";
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
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ self.overlays.default ];
      config.allowBroken = true;
    };
  in {
    devShells.default = pkgs.haskellPackages.shellFor {
      packages = p: [ p.gtk-scaling-image ];
      nativeBuildInputs = [ pkgs.pkg-config ] ++ (with pkgs.haskellPackages; [
        cabal-install
        haskell-language-server
        ormolu
      ]);
    };
    packages.default = pkgs.haskellPackages.gtk-scaling-image;
  }) // {
    overlays.default = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
          gtk-scaling-image =
            hself.callCabal2nix "gtk-scaling-image"
            (git-ignore-nix.lib.gitignoreSource ./.)
            { };
        });
      });
    };
  };
}
