_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
      (self: super: {
        xdg-desktop-entry = self.callCabal2nix "xdg-desktop-entry" ./. { };
      });
  });
}
