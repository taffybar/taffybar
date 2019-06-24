self: super:

let
  sourceTransformer = if builtins.getEnv "CI" == "" then builtins.fetchGit else (x: x);
  taffybarOverlay = _: pkgs: {
    haskellPackages = pkgs.haskellPackages.override (old: {
      overrides =
        pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
        (self: super: {
          taffybar =
            self.callCabal2nix "taffybar" (sourceTransformer ./.)
            { inherit (pkgs) gtk3; };
        });
    });
  };
in super.lib.composeExtensions taffybarOverlay (import ./environment.nix) self super
