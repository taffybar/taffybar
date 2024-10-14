# Running Taffybar

Being a desktop component, Taffybar has various runtime dependencies,
depending on your configuration.

## X11 Compisitor

Run an X11 compositor such as [Picom][] for transparency and rounded
window corners.

This is optional and Taffybar looks fine without these features.

[Picom]: https://picom.app/

## XMonad

Example: [`xmonad.hs`](https://github.com/taffybar/taffybar/blob/master/example/xmonad.hs)

Your XMonad configuration should have:
1. [`XMonad.Hooks.ManageDocks.docks`][ManageDocks] - so that Taffybar windows are managed like docks
2. [`XMonad.Hooks.ManageDocks.avoidStruts`][ManageDocks] - so that other windows don't cover Taffybar.
1. [`XMonad.Hooks.EwmhDesktops.ewmh`][EwmhDesktops] - so that Taffybar knows about workspaces and windows managed by XMonad.
2. [`XMonad.Hooks.TaffybarPagerHints.pagerHints`][TaffybarPagerHints] - so that Taffybar knows about the current layout used by XMonad, and which workspaces are visible.

[ManageDocks]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Hooks-ManageDocks.html
[EwmhDesktops]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Hooks-EwmhDesktops.html
[TaffybarPagerHints]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Hooks-TaffybarPagerHints.html

## D-Bus

Taffybar connects to both the session bus and system bus. These days
it would be a rare Linux system which doesn't have a working D-Bus
system bus.

However there can sometimes be problems configuring the per-user
session bus. Ensure that:

1. The D-Bus session bus (i.e. `dbus-daemon` or `dbus-broker`) is
   running for the user, or is able to be socket-activated. If you are
   using `gnome-session` then D-Bus is guaranteed to be running.

2. The `taffybar` process has the `DBUS_SESSION_BUS_ADDRESS`
   environment variable set correctly.
   
   It can happen that Taffybar doesn't have `DBUS_SESSION_BUS_ADDRESS`
   if it is started from an `xsession` script before `dbus-launch`.
   
   Or it can happen if Taffybar is running as a `systemd --user`
   service, and the service gets started before
   `DBUS_SESSION_BUS_ADDRESS` is imported into the systemd manager
   environment.

## Battery Status

UPower is required for [`System.Taffybar.Information.Battery`][Battery].

To test that it's running and working, run:

```
$ upower -i /org/freedesktop/UPower/devices/DisplayDevice
  power supply:         yes
  updated:              Sun 13 Oct 2024 09:13:24 (35002 seconds ago)
  has history:          no
  has statistics:       no
  battery
    present:             yes
    state:               fully-charged
    warning-level:       none
    energy:              49.9776 Wh
    energy-full:         49.9776 Wh
    energy-rate:         0.0076 W
    charge-cycles:       N/A
    percentage:          100%
    icon-name:          'battery-full-charged-symbolic'
```

[Battery]: https://hackage.haskell.org/package/taffybar/docs/System-Taffybar-Information-Battery.html

## System Tray

Run [`status-notifier-watcher`](https://github.com/taffybar/status-notifier-item)
to track registration/deregistration of [StatusNotifierItem (SNI)][sni]
tray icons. This is an implementation of the StatusNotifierWatcher
interface which runs separately to Taffybar, so that tray icons can
survive restarts of Taffybar.

[`System.Taffybar.Widget.SNITray.sniTrayNew`][SNITray] uses D-Bus to ask
StatusNotifierWatcher for the list of tray icons. Therefore, if using
[SNITray][], ensure that `status-notifier-watcher` is started before
Taffybar.

[sni]: https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/
[SNITray]: https://hackage.haskell.org/package/taffybar/docs/System-Taffybar-Widget-SNITray.html

## NixOS

### GDK pixbuf loaders cache

Under NixOS, it's not possible to have a global mutable file such as
`/usr/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache`.

So graphical applications using `gdk-pixbuf` on NixOS need to have the
environment variable `GDK_PIXBUF_MODULE_FILE` set according to the
system configuration. _and_ available in the process execution
environment _before_ they are started.

Applications started from within `gnome-session`, being child
processes of it, will naturally have `GDK_PIXBUF_MODULE_FILE` in their
process environment.

Applications run as `systemd --user` services will not necesarily have
any environment variables at all.

So if you run Taffybar as a `systemd --user` service, then add this to
your NixOS configuration:

```nix
services.xserver.displayManager.importedVariables = [
  "GDK_PIXBUF_MODULE_FILE"
]
```

If using home-manager, and the option [`services.taffybar.enable`](https://github.com/nix-community/home-manager/blob/master/modules/services/taffybar.nix),
this is done for you.

### `XDG_DATA_DIRS`

For loading of desktop entry files and icons, Taffybar needs to be run
with a correctly configured `XDG_DATA_DIRS` environment variable.

If using home-manager and the option [`xsession.enable`](https://github.com/nix-community/home-manager/blob/master/modules/xsession.nix), this is done for you.
