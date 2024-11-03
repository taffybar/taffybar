# A nixpkgs overlay which provides haskellPackages.taffybar.

final: prev: {
  lib = prev.lib.extend (import ./lib-overlay.nix);

  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions
      prev.haskell.packageOverrides
      (final.lib.taffybar.haskellPackageOverrides final prev);
  };
}
