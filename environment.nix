_: pkgs:
let
  addGObjectIntrospection = hpackage: pkgs.haskell.lib.overrideCabal hpackage (current: {
    libraryPkgconfigDepends =
      current.libraryPkgconfigDepends ++ [ pkgs.gobject-introspection ];
  });
in {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      gtk-sni-tray = self.callHackageDirect {
        pkg = "gtk-sni-tray";
        ver = "0.1.6.2";
        sha256 = "13m6klwx0nc58h7fjss2jwcmk2y8kakcrrc741vbfnnap83achv5";
      } {inherit (pkgs) gtk3;};
      dyre = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
        pkg = "dyre";
        ver = "0.9.1";
        sha256 = "sha256-3ClPPbNm5wQI+QHaR0Rtiye2taSTF3IlWgfanud6wLg=";
      } { });
    });
  });
}
