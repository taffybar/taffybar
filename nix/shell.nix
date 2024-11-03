{ pkgs ? import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; } }:

pkgs.haskellPackages.shellFor {
  packages = p: [ p.taffybar p.my-taffybar ];

  withHoogle = true;

  # Add some development tools to the shell.
  nativeBuildInputs = (with pkgs; [
    cabal-install
    haskell-language-server
  ]) ++ (with pkgs.haskellPackages; [
    hlint ormolu weeder implicit-hie hie-bios
  ]);

  inherit (pkgs.haskellPackages.taffybar.env) shellHook;
}
