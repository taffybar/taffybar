# Taffybar

[![Build Status](https://github.com/taffybar/taffybar/actions/workflows/nix.yml/badge.svg)](https://github.com/taffybar/taffybar/actions/workflows/nix.yml) [![Hackage](https://img.shields.io/hackage/v/taffybar.svg?logo=haskell&label=taffybar)](https://hackage.haskell.org/package/taffybar) [![Commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=taffybar%2A&sort=date&label=unreleased%20commits)](https://github.com/taffybar/taffybar/releases/latest) [![Stackage LTS](http://stackage.org/package/taffybar/badge/lts)](http://stackage.org/lts/package/taffybar) [![Stackage Nightly](http://stackage.org/package/taffybar/badge/nightly)](http://stackage.org/nightly/package/taffybar) [![Matrix Chat](https://img.shields.io/matrix/taffybar:matrix.org)](https://matrix.to/#/#taffybar:matrix.org) [![Gitter Chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/taffybar/Lobby) [![License BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/taffybar/taffybar/blob/master/LICENSE)

## Summary
 
Taffybar is a desktop information bar, intended primarily for use with
[XMonad][], though it can also function alongside other EWMH compliant window
managers. It is similar in spirit to [xmobar][], but it differs in that it gives
up some simplicity for a reasonable helping of [GTK 3][] eye candy.

Taffybar also supports running under Wayland via `gtk-layer-shell`. Wayland
support is currently compositor-specific for some widgets, with Hyprland
workspaces, windows, and layout widgets available. Many widgets still rely on
X11/EWMH, so check the widget docs and examples (including
`example/taffybar-wayland.hs`) for what works on your setup.

## Package Release Status

| Package | Hackage | Unreleased commits |
| --- | --- | --- |
| `taffybar` | [![Hackage](https://img.shields.io/hackage/v/taffybar.svg?logo=haskell&label=taffybar)](https://hackage.haskell.org/package/taffybar) | [![Unreleased commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=taffybar%2A&sort=date&label=taffybar%20unreleased)](https://github.com/taffybar/taffybar/releases/latest) |
| `dbus-hslogger` | [![Hackage](https://img.shields.io/hackage/v/dbus-hslogger.svg?logo=haskell&label=dbus-hslogger)](https://hackage.haskell.org/package/dbus-hslogger) | [![Unreleased commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=dbus-hslogger%2A&sort=date&label=dbus-hslogger%20unreleased)](https://github.com/taffybar/taffybar/releases?q=dbus-hslogger) |
| `dbus-menu` | [![Hackage](https://img.shields.io/hackage/v/dbus-menu.svg?logo=haskell&label=dbus-menu)](https://hackage.haskell.org/package/dbus-menu) | [![Unreleased commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=dbus-menu%2A&sort=date&label=dbus-menu%20unreleased)](https://github.com/taffybar/taffybar/releases?q=dbus-menu) |
| `gi-wireplumber` | [![Hackage](https://img.shields.io/hackage/v/gi-wireplumber.svg?logo=haskell&label=gi-wireplumber)](https://hackage.haskell.org/package/gi-wireplumber) | [![Unreleased commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=gi-wireplumber%2A&sort=date&label=gi-wireplumber%20unreleased)](https://github.com/taffybar/taffybar/releases?q=gi-wireplumber) |
| `gtk-scaling-image` | [![Hackage](https://img.shields.io/hackage/v/gtk-scaling-image.svg?logo=haskell&label=gtk-scaling-image)](https://hackage.haskell.org/package/gtk-scaling-image) | [![Unreleased commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=gtk-scaling-image%2A&sort=date&label=gtk-scaling-image%20unreleased)](https://github.com/taffybar/taffybar/releases?q=gtk-scaling-image) |
| `gtk-sni-tray` | [![Hackage](https://img.shields.io/hackage/v/gtk-sni-tray.svg?logo=haskell&label=gtk-sni-tray)](https://hackage.haskell.org/package/gtk-sni-tray) | [![Unreleased commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=gtk-sni-tray%2A&sort=date&label=gtk-sni-tray%20unreleased)](https://github.com/taffybar/taffybar/releases?q=gtk-sni-tray) |
| `gtk-strut` | [![Hackage](https://img.shields.io/hackage/v/gtk-strut.svg?logo=haskell&label=gtk-strut)](https://hackage.haskell.org/package/gtk-strut) | [![Unreleased commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=gtk-strut%2A&sort=date&label=gtk-strut%20unreleased)](https://github.com/taffybar/taffybar/releases?q=gtk-strut) |
| `status-notifier-item` | [![Hackage](https://img.shields.io/hackage/v/status-notifier-item.svg?logo=haskell&label=status-notifier-item)](https://hackage.haskell.org/package/status-notifier-item) | [![Unreleased commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=status-notifier-item%2A&sort=date&label=status-notifier-item%20unreleased)](https://github.com/taffybar/taffybar/releases?q=status-notifier-item) |
| `xdg-desktop-entry` | [![Hackage](https://img.shields.io/hackage/v/xdg-desktop-entry.svg?logo=haskell&label=xdg-desktop-entry)](https://hackage.haskell.org/package/xdg-desktop-entry) | [![Unreleased commits](https://img.shields.io/github/commits-since/taffybar/taffybar/latest/master.svg?filter=xdg-desktop-entry%2A&sort=date&label=xdg-desktop-entry%20unreleased)](https://github.com/taffybar/taffybar/releases?q=xdg-desktop-entry) |

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
