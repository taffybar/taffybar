# Installation

- [#535 - Fix up README](https://github.com/taffybar/taffybar/issues/535)

## Distribution Packaging

Several Linux distributions package Taffybar:

- [nixos](https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/window-managers/taffybar/default.nix)
- [arch/aur](https://aur.archlinux.org/packages/taffybar/)
- [debian](https://aur.archlinux.org/packages/taffybar/)

Of these, only the NixOS distribution is officially supported by the
maintainers. Using any of the others should be pretty similar to using
a bare cabal installation of taffybar.

### NixOS

If you wish to use the NixOS package, make sure that you are using
the top-level [`pkgs.taffybar`](https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/window-managers/taffybar/default.nix)
and not simply `pkgs.haskellPackages.taffybar`.

The top-level package (`pkgs.taffybar`) provides an environment for
Dyre containing `ghc` and libraries for compiling the configuration.

If you need to add additional Haskell dependencies, then override the
`packages` parameter. For example:

```nix
pkgs.taffybar.override {
  packages = hp: [ hp.xmonad-contrib hp.dbus ];
}
```

It is also possible to run/use taffybar on NixOS without using this module by
using a standalone haskell project for the taffybar configuration.

# Installation From Hackage/Source

## Prerequisites

If you are not using distribution packaging of taffybar or the nix package
manager (it will handle getting all the necessary packages for you), you will
need to install all of taffybar's non-haskell dependencies manually.

Start by making sure you have installed everything that is needed for [haskell-gi](https://github.com/haskell-gi/haskell-gi). Taffybar also needs the
equivalent of `libdbusmenu-gtk3-dev` and `libgirepository1.0-dev` on Debian.

You can also get some idea of what the necessary dependencies are by looking at
the `nix` section of the `stack.yaml` file in the taffybar repository.

## Cabal

Cabal installation is a simple matter of installing taffybar from Hackage:

```
cabal install taffybar
```

You do not need to do this if you are using the project approach with cabal.
