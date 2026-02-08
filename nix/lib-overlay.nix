# This is an overlay for nixpkgs.lib.
# The most important part is lib.taffybar.haskellPackageOverrides
# which provides an overlay for haskellPackages.

final: prev: let lib = final; in {
  taffybar = lib.makeExtensible (taffybarFinal: {
    # Define a haskellPackages overlay with taffybar added.
    haskellPackageOverrides = import ./haskell-package-overrides.nix {
      inherit (taffybarFinal) sourceOverrides;
    };

    sourceOverrides = {
      # This package, with non-Haskell files filtered out to
      # reduce unnecessary rebuilds.
      taffybar = lib.fileset.toSource {
        root = ../.;
        fileset = lib.fileset.fileFilter lib.taffybar.taffyFileFilter ../.;
      };
      # Example project
      my-taffybar = ../example;
    };

    haskellFilesFilter = file:
      lib.lists.any file.hasExt [ "cabal" "hs" "hsc" "lhs" "c" "h" ] ||
      lib.strings.hasPrefix "LICENSE" file.name;
    taffyExtraFilesFilter = file:
      lib.lists.any file.hasExt [ "md" "css" "png" "svg" "xml" "golden" ];
    taffyFileFilter = file: final.taffybar.haskellFilesFilter file ||
                            final.taffybar.taffyExtraFilesFilter file;
  });
}
