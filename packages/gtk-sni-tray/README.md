gtk-sni-tray
============
![Build Status](https://github.com/taffybar/gtk-sni-tray/actions/workflows/build.yml/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/gtk-sni-tray.svg?logo=haskell&label=gtk-sni-tray)](https://hackage.haskell.org/package/gtk-sni-tray) [![Stackage LTS](http://stackage.org/package/gtk-sni-tray/badge/lts)](http://stackage.org/lts/package/gtk-sni-tray) [![Stackage Nightly](http://stackage.org/package/gtk-sni-tray/badge/nightly)](http://stackage.org/nightly/package/gtk-sni-tray)

gtk-sni-tray provides a [StatusNotifierHost](https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/StatusNotifierHost/) widget written using the gtk+3 bindings for haskell provided by [gi-gtk](https://hackage.haskell.org/package/gi-gtk). It also provides a simple standalone executable, `gtk-sni-tray-standalone`, that is configured with command line arguments. This executable will run the aforementioned widget by itself in a strut window on X11, or (when available) a layer-shell surface on Wayland.

taffybar
--------
It is generally recommeneded that you use this widget through [taffybar](https://github.com/travitch/taffybar) with [this module](https://github.com/travitch/taffybar/blob/master/src/System/Taffybar/Widget/SNITray.hs), which will allow you to combine it with other useful widgets, and will allow more flexibility in configuration.

StatusNotifierWatcher
---------------------
By default, it is assumed that you are running an isolated StatusNotifierWatcher daemon. [status-notifier-item](https://github.com/IvanMalison/status-notifier-item) provides a StatusNotifierWatcher executable that you can use for this purpose. If you get an error like

```
MethodError {methodErrorName = ErrorName "org.freedesktop.DBus.Error.ServiceUnknown", methodErrorSerial = Serial 7, methodErrorSender = Just (BusName "org.freedesktop.DBus"), methodErrorDestination = Just (BusName ":1.549"), methodErrorBody = [Variant "The name org.kde.StatusNotifierWatcher was not provided by any .service files"]}
```

when you start `gtk-sni-tray-standalone` it is probably because you have not started a StatusNotifierWatcher on your system. You can solve this problem by passing the `--watcher` flag to `gtk-sni-tray-standalone`, but this is not recommeneded, because many SNI processes do not monitor for new watcher processes, and so may not immediately register when this new watcher is started.

Installation
------------

[`nix`](https://nixos.org/download.html),
[`stack`](https://docs.haskellstack.org/en/stable/README/) and
[`cabal`](https://www.haskell.org/cabal/download.html) can all be used to
install gtk-sni-tray.

When building with `cabal`, you will need the following system dependencies
available via `pkg-config`:

* `gtk+-3.0`
* `gtk-layer-shell-0` (for the standalone Wayland layer-shell window)

For Nix users, this repository provides a flake dev shell. If you use `direnv`,
`direnv allow` then `direnv reload` should set up the environment.

If you see a Cabal error about missing pkg-config packages, `scripts/cabal-run`
does a quick preflight check and prints a more direct message.

Development
-----------

Formatting is done with `ormolu` (available in the flake dev shell):

```sh
scripts/fmt
scripts/fmt-check
```
