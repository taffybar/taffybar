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
    # Fix gtk3 build failure due to deprecated threading APIs being treated as errors
    gtk3 = lib.pipe super.gtk3 [
      (appendConfigureFlags ["--ghc-option=-optc=-Wno-error=deprecated-declarations"])
    ];
    taffybar = lib.pipe super.taffybar [
      (self.generateOptparseApplicativeCompletions [ "taffybar" ])
      (overrideCabal (drv: {
        librarySystemDepends =
          (drv.librarySystemDepends or []) ++ [ pkgs.gtk4-layer-shell ];
      }))
      (with pkgs.xorg; addTestToolDepends [
        xorgserver   # Provides Xvfb
        xprop        # Used for test assertions
        xrandr       # Used for test setup
        pkgs.xdummy  # Wrapper script and config for Xserver
        pkgs.xterm   # An X client
        pkgs.dbus    # dbus-daemon for test environment
        (pkgs.python3.withPackages (ps: with ps; [
          python-dbusmock  # Mock system bus services such as UPower
        ]))
        pkgs.upower  # For checking the mock system bus service
      ])
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

  fixDeps92 = self: super: lib.optionalAttrs (lib.versionOlder super.ghc.version "9.4") {
    taffybar = super.taffybar.override { inherit (pkgs) gtk4; };
    gtk4-sni-tray = super.gtk4-sni-tray.override { inherit (pkgs) gtk4; };
  };

in
  lib.composeManyExtensions [
    (haskellLib.packageSourceOverrides sourceOverrides)
    configuration
    fixDeps92
  ]
