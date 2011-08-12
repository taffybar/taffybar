This is a desktop information bar intended for use with XMonad and
similar window managers.  It is similar in spirit to xmobar; it is
different in that it gives up some simplicity for a reasonable helping
of eye candy.  This bar is based on GTK+ (via gtk2hs) and uses fancy
graphics where doing so is reasonable and useful.

The bar is configured much like XMonad.  It uses
~/.config/taffybar/taffybar.hs as its configuration file.  This file
is just a Haskell program that invokes the real _main_ function with a
configuration object.  The configuration file basically just specifies
which widgets to use, though any arbitrary Haskell code can be
executed before the bar is created.

There are some generic pre-defined widgets available:

 * Graph (modeled after the graph widget in Awesome)
 * Vertical bar (also similar to a widget in Awesome)
 * Periodically-updating labels, graphs, and vertical bars

There are also several more specialized widgets:

 * Battery widget
 * Textual clock
 * Freedesktop.org notifications (via dbus)
 * MPRIS widget (currently only supports MPRIS1)
 * Weather widget
 * XMonad log widget (listens on dbus instead of stdin)
 * System tray

TODO
====

An incomplete list of things that would be cool to have:

 * xrandr widget (for dealing changing clone/extend mode and orientation)
 * MPRIS2 widget
 * Better behavior when adding/removing monitors (never tried it)
 * Make MPRIS more configurable
