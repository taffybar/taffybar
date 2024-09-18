{ pkgs ? import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; } }:

pkgs.haskellPackages.shellFor {
  packages = p: [ p.taffybar p.my-taffybar ];

  withHoogle = true;

  # Add some development tools to the shell.
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.haskell-language-server
  ] ++ (with pkgs.haskellPackages; [
    hlint ormolu implicit-hie hie-bios
  ]);

  inherit (pkgs.haskellPackages.taffybar.env) shellHook;
}
