# 0.4.0

## Features

 * Resize the bar when the screen configuration changes (Robert Helgesson)
 * Support bypassing `dyre` by exposing `taffybarMain` (Christian Hoener zu Siederdissen)
 * Textual CPU and memory monitors (Zakhar Voit)
 * A new window switcher menu in the pager (José Alfredo Romero L)
 * Dynamic workspace support in the workspace switcher (Nick Hu)
 * More configurable network monitor (Arseniy Seroka)

## Bug Fixes

 * UTF8 fixes (Nathan Maxson)
 * Various fixes to EWMH support (José Alfredo Romero L)

## Deprecations

The `XMonadLog` module is deprecated.  This module let taffybar display XMonad desktop information through a dbus connection.  The EWMH desktop support by José Alfredo Romero L is better in every way, so that (through TaffyPager) is the recommended replacement.  Upgrading should be straightforward.
