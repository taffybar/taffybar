{
  inputs.gitIgnoreNix = {
    url = github:IvanMalison/gitignore.nix/master;
  };
  outputs = { self, nixpkgs, gitIgnoreNix }: {
    defaultPackage.x86_64-linux = (import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
      }).haskellPackages.taffybar;
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {})) (s: super: {
          taffybar =
            s.callCabal2nix "taffybar" (gitIgnoreNix.gitIgnoreSource ./.)
            { inherit (prev) gtk3; };
          gtk-sni-tray = s.callHackageDirect {
            pkg = "gtk-sni-tray";
            ver = "0.1.6.2";
            sha256 = "13m6klwx0nc58h7fjss2jwcmk2y8kakcrrc741vbfnnap83achv5";
          } { inherit (prev) gtk3; };
          dyre = prev.haskell.lib.dontCheck (s.callHackageDirect {
            pkg = "dyre";
            ver = "0.9.1";
            sha256 = "sha256-3ClPPbNm5wQI+QHaR0Rtiye2taSTF3IlWgfanud6wLg=";
          } { });
        });
      });
    };
  };
}
