# This file defines an overlay for haskellPackages.
#
# Haskell package sources are given in the "sourceOverrides" argument.
{ sourceOverrides }:

# Then, to produce the actual overlay for haskellPackages, it must be
# applied to the "final" and "prev" args of a nixpkgs overlay.
final: prev: let
  pkgs = final;
  inherit (pkgs) lib;

  haskellLib = pkgs.haskell.lib.compose;

  # Add further customization of haskellPackages here
  configuration = with haskellLib; self: super: {
    taffybar = lib.pipe super.taffybar [
      (self.generateOptparseApplicativeCompletions [ "taffybar" ])
      (overrideCabal (drv: {
        # This is required so that "cabal repl" and haskell-language-server
        # can find non-pkgconfig dependencies.
        shellHook = ''
          ${drv.shellHook or ""}
          export LD_LIBRARY_PATH=${lib.makeLibraryPath [ pkgs.zlib ]}:$LD_LIBRARY_PATH
        '';
      }))

      # Add checks as passthru attributes
      (overrideCabal (drv: {
        passthru = drv.passthru // rec {
          fail-on-all-warnings = self.taffybar-fail-on-all-warnings;

          # Generates the HLint report but won't fail if there are hints.
          hlint-report = pkgs.runCommand "${drv.pname}-hlint-${drv.version}" {
            inherit (drv) src;
            nativeBuildInputs = [ pkgs.hlint ];
          } ''
            cd $src
            mkdir $out
            hlint . -j$NIX_BUILD_CORES --report=$out/report.html --no-exit-code --json > $out/report.json
          '';

          # Reads the above HLint report and fails if there are hints.
          hlint = pkgs.runCommand "${drv.pname}-check-hlint-${drv.version}" {
            src = hlint-report;
            nativeBuildInputs = [ pkgs.jq ];
          } ''
            echo "Checking $src/report.json"
            jq -r 'length|if .==0 then "No hints." else "\(.) hint\(if .>1 then "s" else "" end).\n"|halt_error end' < $src/report.json
            ln -s $src $out
         '';
        };
      }))
    ];

    taffybar-fail-on-all-warnings = lib.pipe self.taffybar [
      failOnAllWarnings
      # Use a different name so we don't confuse it with the normal build.
      (overrideCabal (drv: { pname = "fail-on-all-warnings-${drv.pname}"; }))
      # Various tricks to try and make the build quicker.
      dontHaddock
      disableLibraryProfiling
      disableSharedLibraries
      disableSharedExecutables
      (appendBuildFlag "--ghc-options=-c")  # compile but don't link
      (overrideCabal (drv: {
        checkPhase = "";
        enableSeparateDataOutput = false;
        installPhase = "";
        postInstall = "";
        preFixup = "";
      }))
    ];
  };
in
  lib.composeExtensions
    (haskellLib.packageSourceOverrides sourceOverrides)
    configuration
