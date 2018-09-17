# Filter source files that are not part of the build
{ lib }:

path: type:

let
  baseName = baseNameOf (toString path);
in
     type != "symlink"
  && baseName != ".stack-work"
  && baseName != ".git"
  && baseName != "default.nix"
  && baseName != "README.md"
  && baseName != "CHANGELOG.md"
  && baseName != "Dockerfile"
  && baseName != "Dockerfile.base"
  && ! lib.hasSuffix ".example" baseName
