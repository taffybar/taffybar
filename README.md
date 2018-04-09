[![Build Status](https://travis-ci.org/travitch/taffybar.svg?branch=master)](https://travis-ci.org/travitch/taffybar)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/taffybar/Lobby)

This is a desktop information bar intended for use with XMonad and
similar window managers.  It is similar in spirit to xmobar; it is
different in that it gives up some simplicity for a reasonable helping
of eye candy.  This bar is based on GTK+ (via gtk2hs) and uses fancy
graphics where doing so is reasonable and useful.  Example:

![](https://github.com/travitch/taffybar/blob/master/doc/screenshot.png)

The bar is configured much like XMonad.  It uses
~/.config/taffybar/taffybar.hs as its configuration file.  This file
is just a Haskell program that invokes the real _main_ function with a
configuration object.  The configuration file basically just specifies
which widgets to use, though any arbitrary Haskell code can be
executed before the bar is created.

There are some generic pre-defined widgets available:

 * Graph (modeled after the graph widget in Awesome)
 * Vertical bar (also similar to a widget in Awesome)
 * Periodically-updating labels, graphs, and vertical bars

There are also several more specialized widgets:

 * Battery widget
 * Volume widget
 * Network activity
 * Textual clock
 * Freedesktop.org notifications (via dbus)
 * MPRIS1 and MPRIS2 widgets
 * Weather widget
 * Workspace, Window and Layout switchers
 * System tray
 * Freedesktop.org menu

[See full documentation of release version here.](https://hackage.haskell.org/package/taffybar)

Installation
============
**NOTE**: `gtk2hs-buildtools` is needed for installations with GHC 8 and above, till there's better support for `setup-depends`.

### Cabal
```
cabal install taffybar
```

### Stack
```
stack install gtk2hs-buildtools
stack install taffybar
```
