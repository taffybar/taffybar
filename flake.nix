{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix.url = github:hercules-ci/gitignore.nix/master;
    gtk-sni-tray.url = github:taffybar/gtk-sni-tray/master;
    gtk-strut.url = github:taffybar/gtk-strut/master;
    xmonad.url = github:xmonad/xmonad/master;
  };
  outputs = {
    self, flake-utils, nixpkgs, git-ignore-nix, gtk-sni-tray, gtk-strut, xmonad
  }:
  let
    defComp = if builtins.pathExists ./comp.nix
      then import ./comp.nix
      else { };
    hoverlay = final: prev: hself: hsuper: let addPcre2 = package: (
        prev.haskell.lib.addExtraLibraries (prev.haskell.lib.addPkgconfigDepend package final.pcre2) [
            final.pcre2
            final.pcre
            final.xorg.libXdmcp
        ]
      );
      in {
        taffybar = prev.haskell.lib.addPkgconfigDepends (
        hself.callCabal2nix "taffybar"
        (git-ignore-nix.lib.gitignoreSource ./.)
        { inherit (final) gtk3; }) [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
          final.fribidi.out
          final.fribidi.dev
          final.libthai.dev
          final.libdatrie.dev
          final.xorg.libXdmcp.dev
          final.libxkbcommon.dev
          final.libepoxy.dev
          final.xorg.libXtst.out
        ];

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

        gi-cairo-render = addPcre2 hsuper.gi-cairo-render;

        gi-cairo = addPcre2 hsuper.gi-cairo;

        gi-glib = addPcre2 hsuper.gi-glib;

        gi-gmodule = addPcre2 hsuper.gi-gmodule;

        gi-gobject = addPcre2 hsuper.gi-gobject;

        gi-atk = addPcre2 hsuper.gi-atk;

        gtk-sni-tray = prev.haskell.lib.addPkgconfigDepends hsuper.gtk-sni-tray [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
          final.fribidi.out
          final.fribidi.dev
          final.libthai.dev
          final.libdatrie.dev
          final.xorg.libXdmcp.dev
          final.libxkbcommon.dev
          final.libepoxy.dev
          final.xorg.libXtst.out
        ];

        gi-dbusmenugtk3 = prev.haskell.lib.addPkgconfigDepends hsuper.gi-dbusmenugtk3 [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
          final.fribidi.out
          final.fribidi.dev
          final.libthai.dev
          final.libdatrie.dev
          final.xorg.libXdmcp.dev
          final.libxkbcommon.dev
          final.libepoxy.dev
          final.xorg.libXtst.out
        ];

        gi-gtk = prev.haskell.lib.addPkgconfigDepends hsuper.gi-gtk [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
          final.fribidi.out
          final.fribidi.dev
          final.libthai.dev
          final.libdatrie.dev
          final.xorg.libXdmcp.dev
          final.libxkbcommon.dev
          final.libepoxy.dev
          final.xorg.libXtst.out
        ];

        gi-gdkx11 = prev.haskell.lib.addPkgconfigDepends hsuper.gi-gdkx11 [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
          final.fribidi.out
          final.fribidi.dev
          final.libthai.dev
          final.libdatrie.dev
          final.xorg.libXdmcp.dev
          final.libxkbcommon.dev
          final.libepoxy.dev
        ];

        gi-gdk = prev.haskell.lib.addPkgconfigDepends hsuper.gi-gdk [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
          final.fribidi.out
          final.fribidi.dev
          final.libthai.dev
          final.libdatrie.dev
          final.xorg.libXdmcp.dev
          final.libxkbcommon.dev
          final.libepoxy.dev
        ];

        gi-gdkpixbuf = prev.haskell.lib.addPkgconfigDepends hsuper.gi-gdkpixbuf [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
        ];

        gi-pango = prev.haskell.lib.addPkgconfigDepends hsuper.gi-pango [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
          final.fribidi.out
          final.fribidi.dev
          final.libthai.dev
          final.libdatrie.dev
          final.xorg.libXdmcp.dev
        ];

        gi-gio = prev.haskell.lib.addPkgconfigDepends hsuper.gi-gio [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
        ];

        gi-harfbuzz = prev.haskell.lib.addExtraLibraries
          (prev.haskell.lib.addPkgconfigDepend hsuper.gi-harfbuzz final.pcre2) [
            final.pcre2
            final.pcre
            final.freetype
          ];

        haskell-gi = addPcre2 hsuper.haskell-gi;

        haskell-gi-base = let haskell-gi-base =
            prev.haskell.lib.addPkgconfigDepend hsuper.haskell-gi-base final.pcre2;
          in prev.haskell.lib.addExtraLibraries haskell-gi-base [
            final.pcre2
            final.pcre
          ];

        gi-xlib = prev.haskell.lib.addExtraLibraries
          (prev.haskell.lib.addPkgconfigDepends hsuper.gi-xlib [final.pcre2 final.xorg.libXdmcp
            final.xorg.libXau.out
            final.xorg.libXau.dev
          ]) [
            final.pcre2
            final.pcre
            final.xorg.libXau.dev
          ];
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
      ];
    };
    buildInputs = [ hpkg.cabal-install ];
    defaultPackage = hpkg.taffybar;
  }) // { inherit overlay overlays; } ;
}
