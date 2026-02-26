let
  pkgs = (import <nixpkgs> {});
  gtk-sni-tray = pkgs.haskellPackages.callCabal2nix "gtk-sni-tray" ./. { inherit (pkgs) gtk3; };
in pkgs.haskellPackages.shellFor {
  packages = _: [gtk-sni-tray];
  buildInputs = [ pkgs.gtk-layer-shell ];
}
