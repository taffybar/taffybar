# 3.2.0

## New Features

 * The Layout widget can now be styled with the css class "layout-label".

 * A new polling label function `pollingLabelWithVariableDelay` that allows for
   variable poll times was added.

## Breaking Changes

 * The file specified in the cssPath parameter in config is now used instead of,
   rather than in addition to the default user config file.

 * All parameters are now passed to `textClockNewWith` as part of the
   ClockConfig it receives. A new mechanism for rounded variable polling should
   allow the clock to always remain accurate (to the precision selected by the
   user) without having a very high polling rate, thus reducing CPU usage.

 * The polling label functions no longer accept a default text parameter.

# 3.1.2

## Updates

 * Weather now uses new uris and https (Kirill Zaborsky)

 * Bump the version of gi-gdkpixbuf, this fixes nixpkgs compilation

# 3.1.0

## New Features

 * A new module Chrome which makes it so that Workspaces can display the
   favicons of the website that the chrome window is currently visiting.

# 3.0.0

## Breaking Changes

 * Taffybar has replaced gtk2hs with gi-gtk everywhere. All widgets must now be
   created with gi-gtk.

# 2.0.0

## Breaking Changes

 * An entirely new config system has been put in place. TaffybarConfig now lives
   in System.Taffybar.Context, but for most users, System.Taffybar.SimpleConfig
   is the configuration interface that should be used.

 * The main entry point to taffybar is now dyreTaffybar instead of
   defaultTaffybar.

 * All widget constructors provided to both config systems must now be of type
   `TaffyIO Gtk.Widget`. If you have an existing `IO Gtk.Widget` you can convert it
   using liftIO. All widgets provided by taffybar are now of type
   `MonadIO m => m Gtk.Widget`, or specialized to `TaffyIO Gtk.Widgets`.

 * The `graphBackgroundColor` and `graphBorderColor` fields are now RGBA
   quadruples instead of RGB triples.

 * Module removals:

   - WorkspaceSwitcher: Workspaces is much more abstract and makes this widget
     redundant.
   - Pager: The Context module solves the problem that Pager solved in a much
     more general way. It also makes it so that the user doesn't even need to
     know about the Pager component at all.
   - TaffyPager: Since you no longer need to explicitly initialize a Pager, it's
     not really very hard to simply add the (Workspaces, Layout, Windows) triple
     to your config any more.
   - XMonadLog: This module has long been deprecated

 * Module moves:

   - Everything in System.Information has been moved to
     System.Information.Taffybar
   - All Widgets that were found in System.Taffybar have been moved to
     System.Taffybar.Widget
   - The helper widgets that were previously located in System.Taffybar.Widgets
     have been moved to System.Taffybar.Widget.Generic

 * Module renames:

	- WorkspaceHUD -> Workspaces
	- WindowSwitcher -> Windows
	- LayoutSwitcher -> Layout
	- ToggleMonitors -> DBus.Toggle

  * Module deprecations:

    - System.Taffybar.Widget.Systray (Use SNITray instead)
	- System.Taffybar.Widget.NetMonitor (Use
      System.Taffybar.Widget.Text.NetworkMonitor instead)

 * Many widgets have subtle interface changes that may break existing configurations.

## New Features

 * Widgets can now be placed in the center of taffybar with the `centerWidgets`
   configuration parameter.

 * taffybar is now transparent by default, but you will need to use a compositor
   for transparency to work. https://github.com/chjj/compton is recommended. If
   you do not want a transparent taffybar set a background color on the class
   `TaffyBox` in taffybar.css.

 * StatusNotifierItem support has been added to taffybar in the SNITray module.

 * Monitor configuration changes are handled automatically. Unfortunately the
   bar must be completely recreated when this happens.

 * New network monitor widgets `System.Taffybar.Widget.Text.NetworkMonitor`
   and `System.Taffybar.Widget.NetworkGraph` were added.

 * All widgets are now exported in `System.Taffybar.Widget`, which should
   eliminate the need to import widgets explicitly.

# 1.0.2

## Bug Fixes

 * Fix long standing memory leak that was caused by a failure to free memory
   allocated for gtk pixbufs.
 * Widgets unregister from X11 event listening.

# 1.0.0

## Breaking Changes

 * Migrate from Gtk2 to Gtk3, which replaces rc theming with css theming (Ivan Malison)

## New Features

 * Support for taffybar on multiple monitors (Ivan Malison)
 * D-Bus toggling of taffybar per monitor (Ivan Malison)
 * A new workspace switcher widget called WorkspaceHUD (Ivan Malison)
 * Support for multiple batteries via ``batteryContextsNew`` (Edd Steel)
 * Add support for IO actions to configure vertical bar widgets
 * Images in WorkspaceSwitcher - images are taken from EWMH via \_NET\_WM_ICON (Elliot Wolk)
 * Preliminary support for i3wm (Saksham Sharma)
 * Support for multiple network interfaces in NetMonitor (Robert Klotzner)
 * Add a pager config field that configures the construction of window switcher titles (Ivan Malison)
 * Quick start script for installing from git with stack (Ivan Malison)
 * Add a volume widget (Nick Hu and Abdul Sattar)
 * Add available memory field to MemoryInfo (Will Price)
 * The freedesktop.org notifications widget now allows for notifications to
   never expire and can handle multiple notifications at once. In particular the
   default formatter now shows the number of pending notifications (Daniel
   Oliveira)
 * Battery bar is more informative (Samshak Sharma)
 * Network monitor speeds are auto formatted to use the most appropriate units (TeXitoi)
 * A new freedesktop.org menu widget (u11gh)

...and many smaller tweaks.

## Bug Fixes

 * Fixes for outdated weather information sources
 * Various styling fixes in the gtkrc code
 * Share a single X11Connection between all components to fix the `user error
   (openDisplay)` error (Ivan Malison)
 * Call initThreads at startup. This fixes ```taffybar-linux-x86_64:
   xcb_io.c:259: poll_for_event: Assertion `!xcb_xlib_threads_sequence_lost'
   failed.``` (Ivan Malison)
 * Add an eventBox to window switcher to allow setting its background (Ivan Malison)
 * #105 Prevent taffybar from crashing when two windows are closed
   simultaneously, or when taffybar otherwise requests data about a window that
   no longer exists.

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
