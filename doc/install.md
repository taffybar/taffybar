# Installation

- [#535 - Fix up README](https://github.com/taffybar/taffybar/issues/535)

## Distribution Packaging

Several Linux distributions package Taffybar:

- [NixOS (nixpkgs)](https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/window-managers/taffybar/default.nix)
- [arch/aur](https://aur.archlinux.org/packages/taffybar/)
- [Debian (main)](https://packages.debian.org/unstable/taffybar)
- [Ubuntu (universe)](https://packages.ubuntu.com/taffybar)

Of these, only the NixOS distribution is officially supported by the
maintainers. Using any of the others would be pretty similar to using
a bare Cabal installation of Taffybar.

### NixOS Package

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

It is also possible to use Taffybar on NixOS without using this
module, for example by using `pkgs.haskellPackages.taffybar` as a
library in another package (see [Configuration (and compilation)](./config.md)).

### Debian/Ubuntu

On Debian/Ubuntu the `taffybar` package contains the executable
only. Install the `libghc-taffybar-dev` package to get the
`System.Taffybar` Haskell library.

The development package `libghc-taffybar-dev` should also pull in the
GHC compiler and other system dependencies such as the `libgtk-3-dev`
package. Therefore it is useful to have, even if you intend to install
Taffybar from source and not use distribution binaries.

# Installation from source

## Prerequisites

If not using a distribution package of Taffybar which handles getting
all the necessary development libraries for you, then you will need to
install all of Taffybar's non-Haskell dependencies manually.

### System Dependencies

Start by making sure you have installed everything that is needed for [haskell-gi](https://github.com/haskell-gi/haskell-gi). Taffybar also needs the
equivalent of [`libdbusmenu-gtk3-dev`](https://packages.debian.org/sid/libdbusmenu-gtk3-dev) and [`libgirepository1.0-dev`](https://packages.debian.org/sid/libgirepository-1.0-dev) on Debian.

You can also get some idea of what the necessary dependencies are by looking at
the `nix` section of the [`stack.yaml`](https://github.com/taffybar/taffybar/blob/master/stack.yaml) file in the Taffybar repository.

### Haskell Compiler

For the greatest chance of success, use one of the GHC versions listed in [`taffybar.cabal` (tested-with)](https://github.com/taffybar/taffybar/blob/master/taffybar.cabal). Currently, GHC 9.6 is a good choice.

## Cabal

Once the prerequisites are in place, Cabal installation is a simple matter of installing [Taffybar from Hackage](https://hackage.haskell.org/package/taffybar):

```
cabal install taffybar
```
