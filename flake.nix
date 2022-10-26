{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix.url = github:hercules-ci/gitignore.nix/master;
    gtk-sni-tray.url = github:taffybar/gtk-sni-tray/master;
    gtk-strut.url = github:taffybar/gtk-strut/master;
  };
  outputs = {
    self, flake-utils, nixpkgs, git-ignore-nix, gtk-sni-tray, gtk-strut
  }:
  let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {

          taffybar =
            hself.callCabal2nix "taffybar"
            (git-ignore-nix.lib.gitignoreSource ./.)
            { inherit (final) gtk3;  };

          dyre = prev.haskell.lib.dontCheck (hself.callHackageDirect {
            pkg = "dyre";
            ver = "0.9.1";
            sha256 = "sha256-3ClPPbNm5wQI+QHaR0Rtiye2taSTF3IlWgfanud6wLg=";
          } { });

        });
      });
    };
    overlays = gtk-strut.overlays ++ gtk-sni-tray.overlays ++ [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.taffybar ];
      nativeBuildInputs = with pkgs.haskellPackages; [
        cabal-install hlint ghcid ormolu implicit-hie haskell-language-server
      ];

      buildInputs = with pkgs; [ pcre.dev util-linux.dev libselinux.dev
        libsepol.dev libthai.dev libdatrie.dev xorg.libXdmcp.dev
        libxkbcommon.dev libepoxy.dev dbus.dev at-spi2-core.dev xorg.libXtst ];

    };
    buildInputs = [ pkgs.haskellPackages.cabal-install ];
    defaultPackage = pkgs.haskellPackages.taffybar;
  }) // { inherit overlay overlays; } ;
}
