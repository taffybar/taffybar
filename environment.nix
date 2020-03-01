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
      xdg-desktop-entry = pkgs.haskell.lib.appendPatch super.xdg-desktop-entry ./xdg-desktop-entry.patch;
    });
  });
}
