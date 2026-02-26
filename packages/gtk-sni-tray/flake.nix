{
  description = "gtk-sni-tray";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    status-notifier-item = {
      url = "github:taffybar/status-notifier-item";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.flake-utils.follows = "flake-utils";
    };
    gtk-strut = {
      url = "github:taffybar/gtk-strut";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.flake-utils.follows = "flake-utils";
    };
    dbus-menu = {
      url = "github:taffybar/dbus-menu";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, status-notifier-item, gtk-strut, dbus-menu }:
  flake-utils.lib.eachDefaultSystem (system: let
    inherit (nixpkgs) lib;
    pkgs = import nixpkgs {
      inherit system;
      overlays = lib.attrValues self.overlays;
      config.allowBroken = true;
    };
    # `cabal run`/`cabal new-run` produces unwrapped binaries. Ensure the dev
    # shell environment can find icon themes and gdk-pixbuf loaders (SVG via
    # librsvg), similar to what Nix wrappers do for installed GTK apps.
    gdkPixbufLoaders = pkgs.symlinkJoin {
      name = "gdk-pixbuf-loaders";
      paths = [ pkgs.gdk-pixbuf pkgs.librsvg ];
    };
    gdkPixbufModulesDir = "${gdkPixbufLoaders}/lib/gdk-pixbuf-2.0/2.10.0/loaders";
    # nix-direnv does not reliably execute `shellHook`, so generate a stable
    # loaders cache in the store and point GDK_PIXBUF_MODULE_FILE at it.
    gdkPixbufLoadersCache = pkgs.runCommand "gdk-pixbuf-loaders.cache" {
      nativeBuildInputs = [ pkgs.gdk-pixbuf.dev ];
    } ''
      export GDK_PIXBUF_MODULEDIR="${gdkPixbufModulesDir}"
      ${pkgs.gdk-pixbuf.dev}/bin/gdk-pixbuf-query-loaders > "$out"
    '';
  in
  {
    # Default to a "wired" Haskell shell so `cabal build` already has a package
    # database with the project's Haskell dependencies (like taffybar does).
    devShells.default = pkgs.haskellPackages.shellFor {
      packages = p: [ p.gtk-sni-tray ];
      buildInputs = with pkgs; [
        gtk3
        gtk-layer-shell
        gobject-introspection
        libdbusmenu-gtk3
        libsysprof-capture
        librsvg
        pcre2.dev

        # Ensure at least standard fallback themes exist in XDG_DATA_DIRS.
        adwaita-icon-theme
        hicolor-icon-theme
      ];
      nativeBuildInputs = (with pkgs; [
        pkg-config
        dbus
      ]) ++ (with pkgs.haskellPackages; [
        cabal-install
        haskell-language-server
      ]);

      # Export as plain env vars so nix-direnv picks them up (no shellHook needed).
      NIX_LDFLAGS = "-fuse-ld=bfd";
      GDK_PIXBUF_MODULEDIR = gdkPixbufModulesDir;
      GDK_PIXBUF_MODULE_FILE = gdkPixbufLoadersCache;
    };

    packages.default = pkgs.haskellPackages.gtk-sni-tray;
  }) // {
    overlays = {
      default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            gtk-sni-tray =
              hself.callCabal2nix "gtk-sni-tray"
              (git-ignore-nix.lib.gitignoreSource ./.)
              { inherit (final) gtk3;  };
          });
        });
      };
      status-notifier-item = status-notifier-item.overlay or status-notifier-item.overlays.default;
      gtk-strut = gtk-strut.overlay or gtk-strut.overlays.default;
      dbus-menu = dbus-menu.overlay or dbus-menu.overlays.default;

      # Keep `direnv reload` / `use flake` working: the devShell ends up building
      # Haskell deps, and status-notifier-item's tests expect `dbus-daemon` to be
      # available on PATH.
      #
      # This overlay name is intentionally lexicographically last so it is
      # applied after the upstream overlays.
      zz-direnv-fixes = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: {}))
            (hself: hsuper: {
              status-notifier-item =
                final.haskell.lib.addBuildTool hsuper.status-notifier-item final.dbus;
            });
        });
      };
    };
  };
}
