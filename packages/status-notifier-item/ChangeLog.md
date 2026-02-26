# Changelog for status-notifier-item

## 0.3.2.10 - 2026-02-17
- Relax upper bounds to restore Stackage nightly compatibility:
  `optparse-applicative < 0.20` and `template-haskell < 2.24`.

## 0.3.2.9 - 2026-02-17
- Host: eliminate duplicate `ItemAdded` deliveries to newly-registered update
  handlers by making handler registration + initial replay atomic with respect
  to item map updates.
- Add a deterministic host integration test that reproduces and guards against
  duplicate `ItemAdded` events from replay/live-update races.

## 0.3.2.7 - 2026-02-13
- Watcher: default `--log-level` is now INFO.
- Watcher: log item/host registrations at INFO; keep per-request method/property
  tracing at DEBUG to avoid spamming.
- Host: downgrade noisy INFO/WARNING logs (signal dumps, handler updates, and
  expected removal/mismatch cases) to DEBUG.

## 0.3.2.6 - 2026-02-13
- Watcher: persist item/host registrations to an XDG cache JSON file and
  restore on startup with validation (bus owner + object/interface checks).
- Watcher: keep persisted state in sync across registrations/unregistrations and
  deduplicate/replace owner-equivalent path registrations.
- Host: reconcile item state when watcher ownership changes so stale items are
  removed after watcher restarts.
- Add integration tests for cache restore, stale-cache pruning, post-restore
  deduplication, and host watcher-owner-change reconciliation.

## 0.3.2.5 - 2026-02-12
- Test suite: start an isolated `dbus-daemon` using a session config shipped
  alongside the `dbus-daemon` executable when available (fixes Nix sandbox
  builds where `/etc/dbus-1/session.conf` is absent).

## 0.3.2.4 - 2026-02-12
- Host: deduplicate items that re-register under a different bus name after a
  watcher restart (e.g. unique name vs well-known name), preventing duplicate
  tray icons.
- Add a regression test for watcher-restart re-registration deduplication.

## 0.3.2.3 - 2026-02-11
- Downgrade unknown-sender update logs from WARNING to DEBUG to avoid noisy
  false alarms for routine tray signals.
- Treat UnknownMethod property refresh failures as expected optional-property
  misses (same as InvalidArgs), logging them at DEBUG unless no updater
  succeeded.
- Add host tests for property update failure log-level classification.

## 0.3.2.2 - 2026-02-11
- Fix watcher registration ownership checks by requiring explicit service-name
  registrations to be initiated by the current owner.
- Fix watcher duplicate handling by coalescing path-first and name-first
  registrations from the same sender/path pair.
- Add an isolated DBus integration test suite covering watcher/host behavior
  and regression tests for registration ownership and deduplication.

## 0.3.2.1 - 2026-02-09
- Add name-owner resolution fallback for signal sender identification, fixing
  noisy errors when items register under well-known bus names.
- Downgrade "Failed to identify sender" log from ERROR to DEBUG when the item
  simply does not implement the getId method.
- Downgrade "Property update failures" log from ERROR to DEBUG when the failures
  are InvalidArgs (item does not support optional properties like OverlayIcon).

## 0.3.2.0 - 2026-02-05
- Report full `bus/path` identifiers for non-default SNI object paths so hosts
  can resolve Ayatana items reliably.
- Accept `bus/path` identifiers in watcher/host lookups for compatibility with
  pathful registrations.
