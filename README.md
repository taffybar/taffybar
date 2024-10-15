# Taffybar

[![Build Status](https://github.com/taffybar/taffybar/actions/workflows/nix.yml/badge.svg)](https://github.com/taffybar/taffybar/actions/workflows/nix.yml) [![Hackage](https://img.shields.io/hackage/v/taffybar.svg?logo=haskell&label=taffybar)](https://hackage.haskell.org/package/taffybar) [![Commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest-release.svg?label=unreleased%20commits)](https://github.com/taffybar/taffybar/compare/latest-release...master) [![Stackage LTS](http://stackage.org/package/taffybar/badge/lts)](http://stackage.org/lts/package/taffybar) [![Stackage Nightly](http://stackage.org/package/taffybar/badge/nightly)](http://stackage.org/nightly/package/taffybar) [![Matrix Chat](https://img.shields.io/matrix/taffybar:matrix.org)](https://matrix.to/#/#taffybar:matrix.org) [![Gitter Chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/taffybar/Lobby) [![License BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/taffybar/taffybar/blob/master/LICENSE)

## Summary
 
Taffybar is a desktop
information bar, intended primarily for use with [XMonad][], though it can also
function alongside other EWMH compliant window managers. It is similar in spirit
to [xmobar][], but it differs in that it gives up some simplicity for a reasonable
helping of [GTK 3][] eye candy.

[![Screenshot](https://raw.githubusercontent.com/taffybar/taffybar/master/doc/screenshot.png)](https://github.com/taffybar/taffybar/blob/master/doc/screenshot.png)

[GTK 3]: https://www.gtk.org/
[XMonad]: https://xmonad.org/
[dwm]: https://dwm.suckless.org/
[xmobar]: https://codeberg.org/xmobar/xmobar
[gi-gtk]: https://hackage.haskell.org/package/gi-gtk
[Haskell]: https://www.haskell.org/
[GHC]: https://www.haskell.org/ghc/

## Taffybar is a library

As with window managers like [XMonad][] and [dwm][], Taffybar is most appropriately
described as a library that allows you to build an executable that is customized
to your tastes. Like [xmobar][] and [XMonad][], Taffybar is configured in [Haskell][].

Taffybar has a reasonably useful default configuration built in.

To use a different configuration, however, you must install a Haskell compiler
([GHC][]) that can compile your [`taffybar.hs`](https://github.com/taffybar/taffybar/blob/master/example/taffybar.hs) source file.

You then select from [the list of available widgets][widgets] for
functionality to add to your Taffybar. If the widget you need doesn't
exist, then create your own with GTK.

[widgets]: http://hackage.haskell.org/package/taffybar/docs/System-Taffybar-Widget.html

## Documentation

* [**Installation**][install]

  Taffybar can be installed from Linux distribution packages,
  or compiled from source.

* [**Configuration** (and compilation)][config]

  Taffybar can recompile itself from `taffybar.hs`, similar to how
  [XMonad][] recompiles itself from `xmonad.hs`.

  There are a number of ways to compile your configuration.

* [**Customization**][custom]

  Taffybar has a library of functions and widgets for collecting and
  displaying information.

  Many aspects of the bar's appearance can be changed using CSS.

* [**Running**][run]

  Taffybar is one component of a desktop environment. To work
  correctly, it requires other desktop components and system services.

* [**FAQ**][faq]

  Assorted information.

* [**Contributing**][contrib]

  [![Help Wanted](https://img.shields.io/github/issues/taffybar/taffybar/help%20wanted.svg)](https://github.com/taffybar/taffybar/labels/help%20wanted)
  [![Easy Issues](https://img.shields.io/github/issues/taffybar/taffybar/easy.svg)](https://github.com/taffybar/taffybar/labels/easy)

  Taffybar desperately needs contributors.
  There is plenty to do; enquire within.
  
[install]: https://github.com/taffybar/taffybar/blob/master/doc/install.md
[config]:  https://github.com/taffybar/taffybar/blob/master/doc/config.md
[custom]:  https://github.com/taffybar/taffybar/blob/master/doc/custom.md
[run]:     https://github.com/taffybar/taffybar/blob/master/doc/run.md
[faq]:     https://github.com/taffybar/taffybar/blob/master/doc/faq.md
[contrib]: https://github.com/taffybar/taffybar/blob/master/doc/contrib.md
