let
  nixpkgs = builtins.fromJSON (builtins.readFile ./.nixpkgs.json);
in
import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgs.rev}.tar.gz";
  inherit (nixpkgs) sha256;
})
