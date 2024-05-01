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
    haskell-language-server = {
      url = github:colonelpanic8/haskell-language-server/goto-dependency-definition-2;
    };
  };
  outputs = {
    self, flake-utils, nixpkgs, git-ignore-nix, gtk-sni-tray, gtk-strut, xmonad, haskell-language-server, ...
  }:
  let
    defComp = if builtins.pathExists ./comp.nix
      then import ./comp.nix
      else { compiler = "ghc96"; };
    hoverlay = final: prev: hself: hsuper:
        let
          cabalEagerPkgConfigWorkaround =
            let
              # Take list of derivations and return list of the transitive dependency
              # closure, only taking into account buildInputs. Loosely based on
              # closePropagationFast.
              propagatedPlainBuildInputs = drvs:
              builtins.map (i: i.val) (
                builtins.genericClosure {
                  startSet = builtins.map (drv:
                  { key = drv.outPath; val = drv; }
                  ) drvs;
                  operator = { val, ... }:
                  if !final.lib.isDerivation val
                  then [ ]
                  else
                  builtins.concatMap (drv:
                  if !final.lib.isDerivation drv
                  then [ ]
                  else [ { key = drv.outPath; val = drv; } ]
                  ) (val.buildInputs or [ ] ++ val.propagatedBuildInputs or [ ]);
                }
              );
            in with final.haskell.lib; compose.overrideCabal (old: {
              benchmarkPkgconfigDepends = propagatedPlainBuildInputs old.benchmarkPkgconfigDepends or [ ];
              executablePkgconfigDepends = propagatedPlainBuildInputs old.executablePkgconfigDepends or [ ];
              libraryPkgconfigDepends = propagatedPlainBuildInputs old.libraryPkgconfigDepends or [ ];
              testPkgconfigDepends = propagatedPlainBuildInputs old.testPkgconfigDepends or [ ];
            });
        in {
          taffybar = cabalEagerPkgConfigWorkaround (prev.haskell.lib.addPkgconfigDepends (
            hself.callCabal2nix "taffybar"
            (git-ignore-nix.lib.gitignoreSource ./.)
            { inherit (final) gtk3; }) [ ]);

            dyre = prev.haskell.lib.dontCheck (hself.callHackageDirect {
              pkg = "dyre";
              ver = "0.9.1";
              sha256 = "sha256-3ClPPbNm5wQI+QHaR0Rtiye2taSTF3IlWgfanud6wLg=";
            } { });

            MissingH = hself.callHackageDirect {
              pkg = "MissingH";
              ver = "1.6.0.0";
              sha256 = "sha256-nSQ6KXbw1hjbiRzm167lMAiEfdpAZUeMrJiD/MH04JE=";
            } { };

            ghcid = hself.callHackageDirect {
              pkg = "ghcid";
              ver = "0.8.8";
              sha256 = "sha256-eT/dVq44WhRAMY6bj1cqmvSXdPM+SHEDo0Sr+ue0kcI=";
            } { };

            xdg-desktop-entry = prev.haskell.lib.dontCheck (hself.callHackageDirect {
              pkg = "xdg-desktop-entry";
              ver = "0.1.1.2";
              sha256 = "sha256-ie0SUvfW/RmsCHs8gcaHDmMNpj2G/GVt5/C24SaCMfA=";
            } { });

            gi-atk = cabalEagerPkgConfigWorkaround hsuper.gi-atk;
            gi-cairo = cabalEagerPkgConfigWorkaround hsuper.gi-cairo;
            gi-cairo-render = cabalEagerPkgConfigWorkaround hsuper.gi-cairo-render;
            gi-dbusmenu = cabalEagerPkgConfigWorkaround hsuper.gi-dbusmenu;
            gi-dbusmenugtk3 = cabalEagerPkgConfigWorkaround hsuper.gi-dbusmenugtk3;
            gi-gdk = cabalEagerPkgConfigWorkaround hsuper.gi-gdk;
            gi-gdkpixbuf = cabalEagerPkgConfigWorkaround hsuper.gi-gdkpixbuf;
            gi-gdkx11 = cabalEagerPkgConfigWorkaround hsuper.gi-gdkx11;
            gi-gio = cabalEagerPkgConfigWorkaround hsuper.gi-gio;
            gi-glib = cabalEagerPkgConfigWorkaround hsuper.gi-glib;
            gi-gmodule = cabalEagerPkgConfigWorkaround hsuper.gi-gmodule;
            gi-gobject = cabalEagerPkgConfigWorkaround hsuper.gi-gobject;
            gi-gtk = cabalEagerPkgConfigWorkaround hsuper.gi-gtk;
            gi-harfbuzz = cabalEagerPkgConfigWorkaround hsuper.gi-harfbuzz;
            gi-pango = cabalEagerPkgConfigWorkaround hsuper.gi-pango;
            gi-xlib = cabalEagerPkgConfigWorkaround hsuper.gi-xlib;
            gtk-sni-tray = cabalEagerPkgConfigWorkaround hsuper.gtk-sni-tray;
            haskell-gi = cabalEagerPkgConfigWorkaround hsuper.haskell-gi;
            haskell-gi-base = cabalEagerPkgConfigWorkaround hsuper.haskell-gi-base;
        };
    overlay = xmonad.lib.fromHOL hoverlay defComp;
    overlays = gtk-strut.overlays ++ gtk-sni-tray.overlays ++ [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
      hpkg = pkgs.lib.attrsets.getAttrFromPath (xmonad.lib.hpath defComp) pkgs;
  in
  {
    inherit defComp hoverlay;
    devShell = hpkg.shellFor {
      packages = p: [ p.taffybar ];
      nativeBuildInputs = with hpkg; [
        cabal-install hlint ormolu implicit-hie
      ];

      buildInputs = with pkgs; [
        pcre.dev pcre2.dev util-linux.dev libselinux.dev
        libsepol.dev libthai.dev libdatrie.dev xorg.libXdmcp.dev
        libxkbcommon.dev libepoxy.dev dbus.dev at-spi2-core.dev xorg.libXtst
      ];
    };
    defaultPackage = hpkg.taffybar;
  }) // { inherit overlay overlays; } ;
  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };
}
