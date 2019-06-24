# Taffybar [![Hackage](https://img.shields.io/hackage/v/taffybar.svg?logo=haskell&label=taffybar)](https://hackage.haskell.org/package/taffybar) [![Commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest-release.svg?label=unreleased%20commits)](https://github.com/taffybar/taffybar/compare/latest-release...master) [![Build Status](https://travis-ci.org/taffybar/taffybar.svg?branch=master)](https://travis-ci.org/taffybar/taffybar) [![Help Wanted](https://img.shields.io/github/issues/taffybar/taffybar/help%20wanted.svg)](https://github.com/taffybar/taffybar/labels/help%20wanted) [![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/taffybar/Lobby) [![License BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/taffybar/taffybar/blob/master/LICENSE)

![https://github.com/taffybar/taffybar/blob/master/doc/screenshot.png](https://raw.githubusercontent.com/taffybar/taffybar/master/doc/screenshot.png)

Taffybar is a gtk+3 [(through
gi-gtk)](https://github.com/taffybar/taffybar/issues/256) based desktop
information bar, intended primarily for use with XMonad, though it can also
function alongside other EWMH compliant window managers. It is similar in spirit
to xmobar, but it differs in that it gives up some simplicity for a reasonable
helping of eye candy.

Before Installing
-----------------

The installation method, and procedure that must be followed depends on whether
or not you intend to setup a new haskell project and use `startTaffybar`, or if
you want to use the `dyreTaffybar` approach. It is important for you to read
this section so you can understand what all of that means before you decide how
you want to install taffybar.

### Taffybar is a library

As with window managers like XMonad and dwm, taffybar is most appropriately
described as a library that allows you to build an executable that is customized
to your tastes. This means that taffybar must be installed alongside a haskell
compiler (ghc) that can compile the user's configuration source file.

### The taffybar binary and `startTaffybar` vs `dyreTaffybar`

Taffybar can be started from your configuration file in two different ways:

#### `dyreTaffybar`

The `dyreTaffybar` entry point to taffybar uses the [dyre
library](https://github.com/willdonnelly/dyre) to automatically recompile your
taffybar configuration whenever it detects that it has changed. The binary that
is distributed with taffybar does nothing more than call this entry point. The
main downside of this approach is that it does not allow the user to use any
sort of project files for their configuration, and they must have any packages
that are necessary for compilation of their configuration available in their
global ghc environment.

#### `startTaffybar`

The `startTaffybar` entry point to taffybar simply starts taffybar with the
provided configuration. The user binary will not be automatically recompiled
when source files change. The advantage of using `startTaffybar` directly is
that you can use that in the main function of a cabal project.

Distribution Packaging
----------------------
Several linux distributions package taffybar
([nixos](https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/window-managers/taffybar/default.nix),
[arch/aur](https://aur.archlinux.org/packages/taffybar/),
[debian](https://aur.archlinux.org/packages/taffybar/)). Of these, only the
NixOS distribution is officially supported by the maintainers. Using any of the
others should be pretty similar to using a bare cabal installation of taffybar.

#### NixOS

If you wish to use the NixOS package for taffybar, make sure that you are using
the
[module](https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/window-managers/taffybar/default.nix),
and not simply the haskellPackage for taffybar. If you need to add additional
haskell packages to the environment that compiles your taffybar.hs you will need
to invoke that module and use the packages parameter to allow this.

##### Using the overlay.nix when taffybar is broken in nixpkgs
The taffybar haskell package in nixpkgs has been broken in the unstable channel
from time to time. To ameliorate this issue, this repository provides an overlay
file which can be used to build taffybar. See [this
comment](https://github.com/taffybar/taffybar/issues/464#issuecomment-503258726)
for details on how to use the overlay.

Installation From Hackage/Source
--------------------------------

### Prerequisites

If you are not using distribution packaging of taffybar or the nix package
manager (it will handle getting all the necessary packages for you), you will
need to install all of taffybar's non-haskell dependencies manually.

Start by making sure you have installed everything that is needed for [haskell-gi](https://github.com/haskell-gi/haskell-gi). Taffybar also needs the
equivalent of `libdbusmenu-gtk3-dev` and `libgirepository1.0-dev` on Debian.

You can also get some idea of what the necessary dependencies are by looking at
the nix section of the stack.yaml file in the taffybar repository.

### Cabal

Cabal installation is a simple matter of installing taffybar from hackage:
```
cabal install taffybar
```

You do not need to do this if you are using the project approach with cabal.

### The project approach
The project approach to installing/using taffybar involves maintaining a small
haskell project that produces the users taffybar binary. No matter which package
manager you choose to use you will need to make a .cabal file for this project.
It is recommended that you use [this
example](https://github.com/taffybar/taffybar/blob/master/example/my-taffybar.cabal)
as a template. In that example, the users configuration resides in the file
`taffybar.hs` in the same directory, but that can be changed as needed. As of
right now, `dyreTaffybar` is incompatible with this approach because dyre simply
calls ghc directly.

### Cabal

Simply run `cabal new-install` to install the binary.

### Stack

With stack, you will also need to maintain a stack.yaml file. Run `stack
install` to install the binary. See [this
example](https://github.com/taffybar/taffybar/blob/master/example/stack.yaml)

### Nix

You will need to add default.nix file to your package. See [this
example](https://github.com/taffybar/taffybar/blob/master/example/default.nix)

You may also need to use the overlay provided by this repository. See [this
comment](https://github.com/taffybar/taffybar/issues/464#issuecomment-503258726)
for details.

#### Overlay

The taffybar haskell package is currently broken in nixpkgs, because some of its
dependencies are not compiling correctly/are not sufficiently new. The
environment.nix file in this repository fixes these build issues with an
overlay. The overlay.nix file extends the environment overlay so that it
overrides the taffybar package's build description to build the nix taffybar
package from the repository source directory. An example of how to set up
nixpkgs to use the taffybar overlay can be found
[here](https://github.com/ivanmalison/dotfiles/blob/a20b11a070472d182e09cf39f2b0149f39eac9ac/dotfiles/config/taffybar/base.nix#L1).


Configuration
-------------

Like xmobar and XMonad, taffybar is configured in haskell. Taffybar depends on
dyre to automatically detect changes to its configuration file
(`$XDG_CONFIG_HOME/taffybar/taffybar.hs`) and recompile when appropriate.

For more details about how to configure taffybar, see the [full
documentation](https://hackage.haskell.org/package/taffybar). You can find a
list of available widgets
[here](http://hackage.haskell.org/package/taffybar-2.0.0/docs/System-Taffybar-Widget.html)

FAQ
---

For the time being, taffybar's frequently asked questions page lives in [this
github issue](https://github.com/taffybar/taffybar/issues/332).

Contributing
------------

Taffybar desperately needs contributors. If you want to help, but don't know
where to get started you can check out our "help wanted" and "easy" labels:


[![Help Wanted](https://img.shields.io/github/issues/taffybar/taffybar/help%20wanted.svg)](https://github.com/taffybar/taffybar/labels/help%20wanted)
[![Help Wanted](https://img.shields.io/github/issues/taffybar/taffybar/easy.svg)](https://github.com/taffybar/taffybar/labels/easy)
