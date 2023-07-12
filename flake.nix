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
  };
  outputs = {
    self, flake-utils, nixpkgs, git-ignore-nix, gtk-sni-tray, gtk-strut, xmonad, ...
  }:
  let
    defComp = if builtins.pathExists ./comp.nix
      then import ./comp.nix
      else { };
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
          taffybar = prev.haskell.lib.addPkgconfigDepends (
            hself.callCabal2nix "taffybar"
            (git-ignore-nix.lib.gitignoreSource ./.)
            { inherit (final) gtk3; }) [ ];

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

            ConfigFile = hself.callHackageDirect {
              pkg = "ConfigFile";
              ver = "1.1.4";
              sha256 = "sha256-DxW9S8IBUc+f3Y/EbNrcZ6Zo0qhAqV7D5nnuB51E1dc=";
            } { };

            gi-cairo-render = cabalEagerPkgConfigWorkaround hsuper.gi-cairo-render;
            gi-cairo = cabalEagerPkgConfigWorkaround hsuper.gi-cairo;
            gi-glib = cabalEagerPkgConfigWorkaround hsuper.gi-glib;
            gi-gmodule = cabalEagerPkgConfigWorkaround hsuper.gi-gmodule;
            gi-gobject = cabalEagerPkgConfigWorkaround hsuper.gi-gobject;
            gi-atk = cabalEagerPkgConfigWorkaround hsuper.gi-atk;
            gtk-sni-tray = cabalEagerPkgConfigWorkaround hsuper.gtk-sni-tray;
            gi-dbusmenugtk3 = cabalEagerPkgConfigWorkaround hsuper.gi-dbusmenugtk3;
            gi-gtk = cabalEagerPkgConfigWorkaround hsuper.gi-gtk;
            gi-gdkx11 = cabalEagerPkgConfigWorkaround hsuper.gi-gdkx11;
            gi-gdk = cabalEagerPkgConfigWorkaround hsuper.gi-gdk;
            gi-gdkpixbuf = cabalEagerPkgConfigWorkaround hsuper.gi-gdkpixbuf;
            gi-pango = cabalEagerPkgConfigWorkaround hsuper.gi-pango;
            gi-gio = cabalEagerPkgConfigWorkaround hsuper.gi-gio;
            gi-harfbuzz = cabalEagerPkgConfigWorkaround hsuper.gi-harfbuzz;
            haskell-gi = cabalEagerPkgConfigWorkaround hsuper.haskell-gi;
            haskell-gi-base = cabalEagerPkgConfigWorkaround hsuper.haskell-gi-base;
            gi-xlib = cabalEagerPkgConfigWorkaround hsuper.gi-xlib;
        };
    overlay = xmonad.lib.fromHOL hoverlay defComp;
    overlays = gtk-strut.overlays ++ gtk-sni-tray.overlays ++ [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
      hpkg = pkgs.lib.attrsets.getAttrFromPath (xmonad.lib.hpath defComp) pkgs;
  in
  rec {
    inherit defComp hoverlay;
    devShell = hpkg.shellFor {
      packages = p: [ p.taffybar ];
      nativeBuildInputs = with hpkg; [
        cabal-install # hlint ormolu implicit-hie haskell-language-server
      ];

      buildInputs = with pkgs; [
        pcre.dev pcre2.dev util-linux.dev libselinux.dev
        libsepol.dev libthai.dev libdatrie.dev xorg.libXdmcp.dev
        libxkbcommon.dev libepoxy.dev dbus.dev at-spi2-core.dev xorg.libXtst
        hpkg.haskell-language-server
      ];
    };
    defaultPackage = hpkg.taffybar;
  }) // { inherit overlay overlays; } ;
}
