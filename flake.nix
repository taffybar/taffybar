{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix = {
      url = github:hercules-ci/gitignore.nix/master;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gtk-sni-tray = {
      url = github:taffybar/gtk-sni-tray/master;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.status-notifier-item.follows = "status-notifier-item";
    };
    gtk-strut = {
      url = github:taffybar/gtk-strut/master;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
    xmonad = {
      url = github:xmonad/xmonad/master;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
    status-notifier-item = {
      url = github:taffybar/status-notifier-item;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
    nixpkgs = {
      url = github:NixOS/nixpkgs/nixos-unstable;
    };
  };

  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, gtk-sni-tray, gtk-strut, status-notifier-item, xmonad }: let
    inherit (nixpkgs) lib;
  in {
    # Define a haskellPackages overlay with taffybar added.
    hoverlay = final: prev: let
      haskellLib = final.haskell.lib.compose;
      inherit (haskellLib) packageSourceOverrides;
    in
      lib.composeExtensions
        (packageSourceOverrides {
          # This package, with non-git files filtered out to reduce
          # unnecessary rebuilds.
          taffybar = git-ignore-nix.lib.gitignoreSource ./.;
          # Example project
          my-taffybar = git-ignore-nix.lib.gitignoreSource ./example;
          # Dependencies which we want to build from source.
          # These are flake inputs.
          inherit gtk-strut gtk-sni-tray status-notifier-item xmonad;
        })
        (self: super: {
          # Add further customization of haskellPackages here
        });

    # Make a nixpkgs overlay using the above haskellPackages overlay.
    overlays.default = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = lib.composeExtensions
          prev.haskell.packageOverrides
          (self.hoverlay final prev);
      };
    };

  } // flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ self.overlays.default ];
      config.allowBroken = true;
    };
    haskellPackages = pkgs.haskell.packages.ghc96;

  in {
    devShells.default = (haskellPackages.shellFor {
      packages = p: [ p.taffybar p.my-taffybar ];

      # Add some development tools to the shell.
      nativeBuildInputs = [
        pkgs.cabal-install
        pkgs.haskell-language-server
      ] ++ (with haskellPackages; [
        hlint ormolu implicit-hie hie-bios
      ]);

    }).overrideAttrs (oldAttrs: {
      # This is required so that "cabal repl" and haskell-language-server
      # can find non-pkgconfig dependencies.
      shellHook = ''
        ${oldAttrs.shellHook or ""}
        export LD_LIBRARY_PATH=${lib.makeLibraryPath [ pkgs.zlib ]}:$LD_LIBRARY_PATH
      '';
    });

    packages = {
      default = self.packages.${system}.taffybar;
      inherit (haskellPackages)
        taffybar
        my-taffybar;
    };
  });

  nixConfig = {
    extra-substituters = [ "https://haskell-language-server.cachix.org" ];
    extra-trusted-public-keys = [ "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8=" ];
  };
}
