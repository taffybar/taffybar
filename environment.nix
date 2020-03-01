_: pkgs:
let
  addGObjectIntrospection = hpackage: pkgs.haskell.lib.overrideCabal hpackage (current: {
    libraryPkgconfigDepends =
      current.libraryPkgconfigDepends ++ [ pkgs.gobject-introspection ];
  });
in {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      status-notifier-item = self.callHackageDirect {
        pkg = "status-notifier-item";
        ver = "0.3.0.5";
        sha256 = "0l7r99k9xdyj4w9iwhcbd32adz6c22zj149cwc97jfsvhhlngld4";
      } { };
      xdg-desktop-entry = self.callHackageDirect {
        pkg = "xdg-desktop-entry";
        ver = "0.1.1.1";
        sha256 = "1ipk17ynx82w2yrxlaz7bhmh6xwd3ahqzmaj27h2s4r4w4sw857r";
      } { };
    });
  });
}
