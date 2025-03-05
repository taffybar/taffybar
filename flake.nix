{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    gtk-sni-tray = {
      url = "github:taffybar/gtk-sni-tray/master";
      flake = false;
    };
    gtk-strut = {
      url = "github:taffybar/gtk-strut/master";
      flake = false;
    };
    xmonad = {
      url = "github:xmonad/xmonad/master";
      flake = false;
    };
    status-notifier-item = {
      url = "github:taffybar/status-notifier-item";
      flake = false;
    };
    weeder-nix = {
      url = "github:NorfairKing/weeder-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gtk-sni-tray, gtk-strut, status-notifier-item, xmonad, weeder-nix }: let
    inherit (self) lib;
    inherit (nixpkgs.lib) composeExtensions;
    inherit (flake-utils.lib) eachSystem;

    supportedSystems = [ "x86_64-linux" "aarch64-linux" ];

    # This flake will generate packages using the following compilers
    # from nixpkgs. The "default" package in this flake will be built with
    # the whichever GHC nixpkgs uses to generate pkgs.haskellPackages.
    # Currently in nixpkgs: haskellPackages = haskell.packages.ghc910
    supportedCompilers = [ "ghc92" "ghc94" "ghc96" "ghc98" ];

  in {
    lib = nixpkgs.lib.extend (composeExtensions
      (import ./nix/lib-overlay.nix)
      (final: prev: {
        taffybar = prev.taffybar.extend (final: prev: {
          sourceOverrides = prev.sourceOverrides // {
            # Flake input dependencies which we want to build from source.
            inherit gtk-strut gtk-sni-tray status-notifier-item xmonad;
          };
        });
      }));

    # Make a nixpkgs overlay using the above haskellPackages overlay.
    overlays.default = composeExtensions
      (import ./nix/overlay.nix)
      (final: prev: {
        lib = prev.lib.extend (final: prev: {
          # Use lib.taffybar from this flake
          inherit (self.lib) taffybar;
        });
      });

  } // eachSystem supportedSystems (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ self.overlays.default ];
      config.allowBroken = true;
    };

  in {
    devShells = {
      default = import ./nix/shell.nix { inherit pkgs; };
    } // lib.genAttrs supportedCompilers
      (compiler: pkgs.haskell.packages.${compiler}.taffybar.env);

    packages = {
      default = self.packages.${system}.taffybar;
      inherit (pkgs.haskellPackages)
        taffybar
        my-taffybar;
    } // lib.listToAttrs (map (compiler: {
      name = "${compiler}-taffybar";
      value = pkgs.haskell.packages.${compiler}.taffybar;
    }) supportedCompilers) // {
      tested-with = pkgs.runCommand "taffybar-tested-with.cabal" {} ''
        echo "tested-with: ${lib.concatMapStringsSep ", " (c: "GHC == ${pkgs.haskell.compiler.${c}.version}") supportedCompilers}" > $out
      '';
    };

    checks = {
      hlint = pkgs.haskellPackages.taffybar.hlint;
      ghc-warnings = pkgs.haskellPackages.taffybar.fail-on-all-warnings;
    } // lib.optionalAttrs (system == "x86_64-linux") {
      dependency-graph = weeder-nix.lib.${system}.makeWeederCheck {
        weederToml = ./weeder.toml;
        haskellPackages = pkgs.haskellPackages;
        packages = [ "taffybar" ];
        # Never fail - too many false positives at the moment.
        reportOnly = true;
      };
    };
  });

  nixConfig = {
    extra-substituters = [ "https://haskell-language-server.cachix.org" ];
    extra-trusted-public-keys = [ "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8=" ];
  };
}
