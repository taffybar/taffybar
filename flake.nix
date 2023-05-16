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
    hpath = { prefix ? null, compiler ? null }:
      (if prefix == null then [] else [ prefix ]) ++
      (if compiler == null
       then [ "haskellPackages" ]
       else [ "haskell" "packages" compiler ]
      );
    hoverlay = final: prev: hself: hsuper: {
      taffybar =
        hself.callCabal2nix "taffybar"
        (git-ignore-nix.lib.gitignoreSource ./.)
        { inherit (final) gtk3;  };

        dyre = prev.haskell.lib.dontCheck (hself.callHackageDirect {
          pkg = "dyre";
          ver = "0.9.1";
          sha256 = "sha256-3ClPPbNm5wQI+QHaR0Rtiye2taSTF3IlWgfanud6wLg=";
        } { });
    };
    overlay = xmonad.lib.fromHOL hoverlay defComp;
    overlays = gtk-strut.overlays ++ gtk-sni-tray.overlays ++ [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
      hpkg = pkgs.lib.attrsets.getAttrFromPath (hpath defComp) pkgs;
  in
  rec {
    devShell = hpkg.shellFor {
      packages = p: [ p.taffybar ];
      nativeBuildInputs = with hpkg; [
        cabal-install hlint ghcid ormolu implicit-hie haskell-language-server
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
