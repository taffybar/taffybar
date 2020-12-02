(import ./default.nix) {
  nixpkgs = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "1121b2259b7d66e8c7c5131d1588a48c80ef9e58";
    sha256 = "0w2i4byhfn8c9lq8a97xnix5alfandqkbyvh6lbpr9zrm63lmyip";
  });
}
