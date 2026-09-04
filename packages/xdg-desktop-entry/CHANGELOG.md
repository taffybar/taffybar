# Revision history for xdg-desktop-entry

## 0.1.1.6 -- 2026-09-04

* Replace the `ini` based parser with one that follows the desktop entry
  specification. Localised keys such as `Name[de]` and `GenericName[ar]` no
  longer make `readDesktopEntry` fail, so nearly every real-world desktop file
  now parses. Files are decoded as UTF-8 regardless of the process locale.
* Drop the `ini` and `unordered-containers` dependencies.

## 0.1.1.5 -- 2026-05-13

* Clean up parser and test warnings for newer GHC releases.

## 0.1.1.4 -- 2026-03-15

* Relax `transformers` upper bound to support `0.6.x`, including GHC 9.12.4 RC snapshots.
* Update package metadata to point at the `taffybar` monorepo subtree.

## 0.1.1.3 -- 2026-02-09

* Relax `ini` upper bound to support 0.5.x (fixes Stackage compatibility)
* Fix partial `head` usage for GHC 9.12 compatibility
* Tested with GHC 9.8.4, 9.10.3, 9.12.3

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
