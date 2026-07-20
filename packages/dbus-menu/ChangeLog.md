# Changelog for dbus-menu

## 0.1.3.4 - 2026-07-20

* Reconcile submenu refreshes by DBusMenu item ID so compatible GTK menu
  widgets survive asynchronous layout updates. This prevents a refresh from
  destroying the pressed item before GTK delivers button release/activation.
* Ignore stale refresh responses and responses for already-destroyed menus.

## 0.1.3.3 - 2026-05-13

* Refresh generated setup metadata and formatting for the monorepo release.

## 0.1.3.2

* Relax `template-haskell` upper bound to `< 2.24` to support GHC 9.12.

## 0.1.3.1

* Make click dispatch truly menu-owned: store the click dispatch table on the
  `Gtk.Menu` GObject so it survives `populateGtkMenu` refreshes and works
  correctly for exported `populateGtkMenu`/`buildGtkMenuItem` APIs.
* Add `gi-gobject` dependency (used to associate dispatch table with menu).

## 0.1.3.0

* Menu-level click dispatch: leaf item actions are registered in a persistent
  dispatch table (shared `IORef` map) owned by the menu, not captured in
  per-widget closures. Individual `onMenuItemActivate` handlers are thin
  trampolines that look up the action at activation time, decoupling dispatch
  from widget lifecycles.

## 0.1.2.0

* Fix submenu clicks being silently dropped: run `AboutToShow` and `GetLayout`
  DBus calls on a forked thread instead of blocking the GTK main loop. Widget
  updates are posted back via `idleAdd` at `PRIORITY_DEFAULT_IDLE` so pending
  click events are processed first.
* Remove redundant `onMenuItemActivate` refresh trigger on parent items (the
  `onWidgetShow` handler on the submenu is sufficient and avoids a double
  rebuild that created additional windows for lost clicks).

## 0.1.1.1

* Fix "menu already attached" GTK warning by removing redundant
  `menuAttachToWidget` call on submenus (superseded by `menuItemSetSubmenu`)

## 0.1.1.0

* Expand module exports (DBusMenu, DBusMenu.Client, DBusMenu.Client.Util)
* Add `dbusmenu-` CSS style classes to menu widgets for theming
* Fix flaky submenu activation by honoring the `children-display` property

## 0.1.0.0

* Initial release, extracted from gtk-sni-tray
