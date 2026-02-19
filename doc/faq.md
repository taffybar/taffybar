# FAQ

This page collects common setup and troubleshooting questions for Taffybar.
It is the successor to
[issue #332 - Add a FAQ section to README/Docs/Wiki](https://github.com/taffybar/taffybar/issues/332).

For detailed runtime setup, also see [Running Taffybar](./run.md).

## What services and runtime prerequisites are required?

If Taffybar starts but parts of it do not work, check these first:

1. D-Bus system and session buses are running.
2. `status-notifier-watcher` is running before Taffybar if you use `SNITray`.
3. `upower` is running if you use battery widgets.
4. A compositor (for example picom) is running if you want transparency/rounded corners.
5. Non-Haskell C dependencies are installed, including `libdbusmenu-gtk3`
   (or distro equivalent), as described in [Installation](./install.md).

## Where should I put `taffybar.hs` and `taffybar.css`?

Use `$XDG_CONFIG_HOME/taffybar/` (usually `~/.config/taffybar/`).

`taffybar.css` placed there is loaded automatically.
Taffybar also watches CSS files for changes and reloads them.

## Why is my tray empty or missing icons?

### Does `SNITray` support XEmbed tray icons?

No. `SNITray` supports StatusNotifierItem/AppIndicator icons, not legacy XEmbed.

For XEmbed-only apps, run a bridge such as:
<https://github.com/KDE/plasma-workspace/tree/master/xembed-sni-proxy>

### Why does `nm-applet` not show up?

Start it with `--indicator`:

```bash
nm-applet --indicator
```

If you use XDG autostart, edit `~/.config/autostart/nm-applet.desktop` and
ensure `Exec=` includes `--indicator`.
If your setup still uses session-management flags, `--sm-disable --indicator`
also works.

Related issues:

- <https://github.com/taffybar/taffybar/issues/491>

## Why do I get "Could not load a pixbuf..." or "null pixbuf" errors?

Most icon/pixbuf failures come from environment or theme setup:

1. Ensure `GDK_PIXBUF_MODULE_FILE` is set correctly.
2. Ensure `XDG_DATA_DIRS` includes directories containing your desktop files and icons.
3. Ensure icon themes are installed (for example `hicolor-icon-theme`, and your chosen GTK theme/icon theme).
4. If menu icons are missing, set `gtk-menu-images = 1` in `~/.config/gtk-3.0/settings.ini`.

Example `settings.ini`:

```ini
[Settings]
gtk-icon-theme-name = Adwaita
gtk-menu-images = 1
```

If you are on NixOS and start Taffybar with `systemd --user`, read the
NixOS sections in [Running Taffybar](./run.md) for
`GDK_PIXBUF_MODULE_FILE` and `XDG_DATA_DIRS` import details.

Some older posts mention `GDK_PIXBUF_ICON_LOADER`; in current Nix/NixOS setup,
the relevant variable is `GDK_PIXBUF_MODULE_FILE`.

Related issues:

- <https://github.com/taffybar/taffybar/issues/367>
- <https://github.com/taffybar/taffybar/issues/373>
- <https://github.com/NixOS/nixpkgs/issues/43836>

## How do I size workspace/tray icons with CSS?

Taffybar icon widgets include CSS classes such as `.auto-size-image`,
`.sni-tray`, and `.window-icon`.

Example:

```css
/* Increase icon vertical size and spacing */
.auto-size-image, .sni-tray {
  padding-top: 4px;
  padding-bottom: 4px;
}

.window-icon {
  opacity: 1;
}
```

Use `GTK_DEBUG=interactive taffybar` to inspect live widget classes in GTK Inspector.

## How do workspace window icon getters work?

By default, workspace icon lookup tries desktop entries, then WM class, then EWMH.
You can override this with `getWindowIconPixbuf`.

Example forcing EWMH icons at a fixed size:

```haskell
import System.Taffybar.Widget.Workspaces

myWorkspacesConfig :: WorkspacesConfig
myWorkspacesConfig =
  defaultWorkspacesConfig
    { getWindowIconPixbuf =
        constantScaleWindowIconPixbufGetter 18 getWindowIconPixbufFromEWMH
    }
```

`defaultGetWindowIconPixbuf` is still the recommended default for most users.

## Why do windows appear in the active workspace on multi-monitor XMonad?

This is usually an older XMonad EWMH implementation issue, not a Taffybar bug.
See:

- <https://github.com/xmonad/xmonad-contrib/pull/238>
- <https://github.com/taffybar/taffybar/issues/239>
- <https://github.com/taffybar/taffybar/issues/369>

Use a recent `xmonad-contrib` that includes the EWMH fix from the PR above.

## Why does Taffybar segfault on older GHC versions?

There were known crash issues around GHC 8.4.x (for example `8.4.2`).
See:

- <https://github.com/taffybar/taffybar/issues/358>

Use a GHC version listed in `tested-with` in `taffybar.cabal`.

## Do you have startup examples (`.xprofile` and `systemd --user`)?

### `.xprofile` example

```bash
# Start tray watcher first when using SNITray
status-notifier-watcher &

# Optional bridge for legacy XEmbed-only tray apps
# xembed-sni-proxy &

# AppIndicator mode
nm-applet --sm-disable --indicator &

taffybar &
```

### `systemd --user` example

`~/.config/systemd/user/taffybar.service`:

```ini
[Unit]
Description=Taffybar
After=graphical-session.target
Wants=graphical-session.target

[Service]
ExecStart=/usr/bin/env taffybar
Restart=on-failure

[Install]
WantedBy=default.target
```

Before starting the unit, import session environment variables once per login:

```bash
systemctl --user import-environment DISPLAY XAUTHORITY \
  DBUS_SESSION_BUS_ADDRESS XDG_DATA_DIRS GDK_PIXBUF_MODULE_FILE
```

On NixOS, you can instead use:
`services.xserver.displayManager.importedVariables`.

## How do I use Cachix with Nix builds?

Taffybar does not require a specific project Cachix cache, but you can use your own:

```bash
nix build .#taffybar
cachix use my-cache
cachix push my-cache "$(readlink -f result)"
```

For team use, add your cache URL/key to Nix `substituters` and
`trusted-public-keys`.

## How do I add click handling to a widget?

Wrap the widget in a `Gtk.EventBox` and handle button press events:

```haskell
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified GI.Gtk as Gtk

makeClickable :: IO Gtk.Widget -> IO () -> IO Gtk.Widget
makeClickable mkInner onClick = do
  inner <- mkInner
  box <- Gtk.eventBoxNew
  Gtk.containerAdd box inner
  Gtk.eventBoxSetVisibleWindow box False
  void $ Gtk.onWidgetButtonPressEvent box $ liftIO onClick >> pure False
  Gtk.widgetShowAll box
  Gtk.toWidget box
```
