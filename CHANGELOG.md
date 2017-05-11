# 0.5

## Breaking Changes

## New Features

 * Support for multiple batteries via ``batteryContextsNew`` (Edd Steel)
 * Add support for IO actions to configure vertical bar widgets
 * Images in the workspace switcher - images are taken from EWMH via \_NET\_WM_ICON (Elliot Wolk)
 * Preliminary support for i3wm (Saksham Sharma)
 * A new workspace switcher widget called WorkspaceHUD (Ivan Malison)
 * Support for multiple network interfaces in NetMonitor (Robert Klotzner)
 * Support for taffybar on multiple monitors (Ivan Malison)
 * Share a single X11Connection between all components to fix the `user error (openDisplay)` error (Ivan Malison)
 * d-bus toggling of taffybar per monitor (Ivan Malison)

## Bug Fixes

 * Fixes for outdated weather information sources
 * Various styling fixes in the gtkrc code

# 0.4.6

 * Fix a longstanding bug in loading .rc files (Peder Stray)
 * Add support for scrolling in the workspace switcher (Saksham Sharma)
 * Improve default formatting of empty workspaces in the pager (Saksham Sharma)
 * Relax gtk version bounds

# 0.4.5

 * GHC 7.10 compat

# 0.4.4

 * Fix compilation with gtk 0.13.1

# 0.4.3

 * Try again to fix the network dependency

# 0.4.2

 * Expand the version range for time
 * Depend on network-uri instead of network

# 0.4.1

 * Make the clock react to time zone changes

# 0.4.0

## Features

 * Resize the bar when the screen configuration changes (Robert Helgesson)
 * Support bypassing `dyre` by exposing `taffybarMain` (Christian Hoener zu Siederdissen)
 * Textual CPU and memory monitors (Zakhar Voit)
 * A new window switcher menu in the pager (José Alfredo Romero L)
 * Dynamic workspace support in the workspace switcher (Nick Hu)
 * More configurable network monitor (Arseniy Seroka)
 * New widget: text-based command runner (Arseniy Seroka)
 * The Graph widget supports lines graphs (via graphDataStyles) (Joachim Breitner)
 * Compile with gtk2hs 0.13

## Bug Fixes

 * Reduce wakeups by tweaking the default GHC RTS options (Joachim Breitner)
 * UTF8 fixes (Nathan Maxson)
 * Various fixes to EWMH support (José Alfredo Romero L)

## Deprecations

The `XMonadLog` module is deprecated.  This module let taffybar display XMonad desktop information through a dbus connection.  The EWMH desktop support by José Alfredo Romero L is better in every way, so that (through TaffyPager) is the recommended replacement.  Upgrading should be straightforward.


# 0.3.0:

 * A new pager (System.Taffybar.TaffyPager) from José A. Romero L.  This pager is a drop-in replacement for the dbus-based XMonadLog widget.  It communicates via X atoms and EWMH like a real pager.  It even supports changing workspaces by clicking on them.  I recommend this over the old widget.
 * Added an MPRIS2 widget (contributed by Igor Babuschkin)
 * Ported to use the newer merged dbus library instead of dbus-client/dbus-core (contributed by CJ van den Berg)
 * Finally have the calendar widget pop up over the date/time widget (contributed by José A. Romero)
 * GHC 7.6 compatibility
 * Vertical bars can now have dynamic background colors (suggested by Elliot Wolk)
 * Bug fixes

# 0.2.1:

 * More robust strut handling for multiple monitors of different sizes (contributed by Morgan Gibson)
 * New widgets from José A. Romero (network monitor, fs monitor, another CPU monitor)
 * Allow the bar widget to grow vertically (also contributed by José A. Romero)

# 0.2.0:

 * Add some more flexible formatting options for the XMonadLog widget (contributed by cnervi).
 * Make the PollingLabel more robust with an exception handler for IOExceptions
 * Added more documentation for a few widgets

# 0.1.3:

 * Depend on gtk 0.12.1+ to be able to build under ghc 7.2
 * Fix the background colors in the calendar so that it follows the GTK theme instead of the bar-specific color settings
 * Fix the display of non-ASCII window titles in the XMonad log applet (assuming you use the dbusLog function)
 * Add a horrible hack to force the bar to not resize to be larger than the screen due to notifications or long window titles

# 0.1.2:

 * Readable widget for freedesktop notifications
 * Fixed a few potential deadlocks on startup
 * Use the GTK+ rc-file styling system for colors instead of hard coding them
