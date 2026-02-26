# Changelog for gtk-sni-tray

## 0.2.0.0

- Breaking: remove `buildTrayWithPriority`, `buildTrayWithPixbufTransform`,
  and `buildTrayWithPriorityAndPixbufTransform`.
- Consolidate tray configuration into `TrayParams` with new fields:
  `trayPriorityConfig`, `trayPixbufTransform`, and `trayEventHooks`.
- Add click interception hooks via `TrayEventHooks`/`TrayClickHook` so callers
  can run custom per-item behavior and choose default/override/consume.

## 0.1.15.0

- Fix: respect the user's GTK icon theme when an SNI item provides IconThemePath
  (and handle IconThemePath values that point inside a theme directory).
- Add icon preference support (choose themed icons vs pixmaps when both are
  provided) and expose it in the standalone executable with `--icon-preference`.
- Add optional icon recoloring in the standalone executable with `--icon-recolor`.
- Nix: improve flake dev shell environment so `cabal new-run` can find icon
  themes and gdk-pixbuf loaders.
- Development: add `scripts/fmt` and `scripts/fmt-check` for ormolu formatting.

## 0.1.14.2

- Relax executable `optparse-applicative` upper bound to `< 0.20` for GHC 9.12 snapshots.
- Bump `dbus-menu` lower bound to `0.1.3.2`.

## 0.1.14.1

- Update the Nix flake lock for `status-notifier-item` to pick up a host-side
  deduplication fix for items that re-register under different bus names after
  watcher restart.

## 0.1.14.0

- Add tray priority matching with a new `TrayPriorityConfig`.
- Add `buildTrayWithPriority` while keeping `buildTray` behavior unchanged.
- Add matcher helpers and combinators so matching can target service name/path,
  menu path, item id/category/status, icon name/title, tooltip text, and
  `itemIsMenu`.

## 0.1.13.2

- Bump `dbus-menu` lower bound to 0.1.3.1.

## 0.1.13.1

- Bump `dbus-menu` lower bound to 0.1.3 for menu-level click dispatch.

## 0.1.13.0

- Use extracted `dbus-menu` library instead of inline DBusMenu implementation.
- Reduce default overlay icon scale from 60% to 40%.
- Fix: clamp scaled pixbuf dimensions to >= 1 to prevent GDK assertion failure.
- Bump `dbus-menu` lower bound to 0.1.1.

## 0.1.12.0

- Add `trayCenterIcons` field to `TrayParams` and `--center-icons` CLI flag to
  center tray icons within the bar.
- Anchor LibDBusMenu popup to the icon image widget instead of the EventBox for
  more accurate popup positioning.

## 0.1.11.3

- Fix menu popup positioning: use `menuPopupAtPointer` with the actual button
  press event (via `getCurrentEvent`) so the menu appears at the click location
  instead of at an incorrect widget-relative position.

## 0.1.11.2

- Add upper bounds to all dependencies per PVP.
- Remove trailing zeros from dependency upper bounds.

## 0.1.11.1

- Fix menu popups on Wayland/layer-shell: use `menuPopupAtWidget` instead of
  `menuPopupAtPointer` which fails with "no trigger event" when the GdkEvent's
  window is not a valid GDK surface.
- Fix menu item clicks: defer menu widget destruction via `idleAdd` with
  `PRIORITY_LOW` so GTK's `activate` signal fires before the widget is destroyed.
- Fix DBus Event variant wrapping: correctly double-wrap the data parameter
  to produce wire type `v` instead of `i`.
- Send Event DBus calls on a forked thread to avoid blocking the GTK main loop.
- Default menu backend to `HaskellDBusMenu` (pure Haskell implementation).
- Deferred popup for `LibDBusMenu` backend to avoid assertion failures from
  showing the menu before the C library finishes loading the layout.

## 0.1.11.0

- Restore `libdbusmenu` (`gi-dbusmenugtk3`) as the default menu backend.
- Add runtime-configurable menu backend selection: `LibDBusMenu` (default) or
  `HaskellDBusMenu` via `trayMenuBackend` in `TrayParams` and `--menu-backend`
  CLI flag.

## 0.1.10.2

- Fix menu popups not appearing: restore `widgetShowAll` so menu items are
  visible, and use `menuPopupAtPointer` for reliable positioning on both X11
  and Wayland.
- Fix all `-Wall` warnings across library and executable.

## 0.1.10.1

- Fix menu popup positioning on Wayland: use `menuAttachToWidget` instead of
  premature `widgetShowAll` so that `menuPopupAtWidget` can properly anchor the
  popup surface to the tray icon.

## 0.1.10.0

- Replace `libdbusmenu` usage (`gi-dbusmenugtk3`) with a pure Haskell
  implementation of the `com.canonical.dbusmenu` protocol.
- Add CSS style classes/names to tray menu widgets to make them themeable.

## 0.1.9.1

- Fix type error with DM.Menu and Gtk.menuPopupAtWidget by adding explicit
  cast to Gtk.Menu. This fixes build failures with newer GI bindings where the
  type hierarchy is not automatically recognized.

## 0.1.9.0

- Use the `gi-gtk3` and `gi-gdk3` build dependencies, which have been
  renamed from `gi-gtk` and `gi-gdk`.
