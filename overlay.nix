self: super:

let
  sourceTransformer = if builtins.getEnv "CI" == "" then builtins.fetchGit else (x: x);
  taffybarOverlay = _: pkgs: with pkgs.haskell.lib; {
    haskellPackages = pkgs.haskellPackages.override (old: {
      overrides =
        pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
        (self: super: {
          taffybar =
            self.callCabal2nix "taffybar" (sourceTransformer ./.)
            { inherit (pkgs) gtk3; };
          # https://github.com/taffybar/gtk-sni-tray/pull/25
          gtk-sni-tray = markUnbroken (appendPatch super.gtk-sni-tray (pkgs.fetchpatch {
            url = "https://github.com/rvl/gtk-sni-tray/commit/4afd84654cb3f2bd2bb7d39451706c5914fd3cdf.patch";
            sha256 = "1xjxlh58vnykqsjq4qw8mliq3gk17mwxi4h9z8dvjyav8zqg05rn";
          }));
        });
    });
  };
in super.lib.composeExtensions taffybarOverlay (import ./environment.nix) self super
