let
  pkgs = (import <nixpkgs> {});
  sourceTransformer = if builtins.getEnv "CI" == "" then builtins.fetchGit else (x: x);
in pkgs.haskellPackages.callCabal2nix "gtk-sni-tray" (sourceTransformer ./.) { inherit (pkgs) gtk3; }
