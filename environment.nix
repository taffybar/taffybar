_: pkgs:
let
  addGObjectIntrospection = hpackage: pkgs.haskell.lib.overrideCabal hpackage (current: {
    libraryPkgconfigDepends =
      current.libraryPkgconfigDepends ++ [ pkgs.gobject-introspection ];
  });
in {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
    });
  });
}
