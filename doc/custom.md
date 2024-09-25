# Customization

For details about the library modules for configuring your Taffybar,
see the [full documentation][hackage].

[hackage]: https://hackage.haskell.org/package/taffybar
[gi-gtk]: https://hackage.haskell.org/package/gi-gtk

- [`taffybar` package documentation][hackage] - `System.Taffybar`
- [`gi-gtk` package documentation][] - `GI.Gtk`

## CSS

[#308 Add styling tips section to README/docs](https://github.com/taffybar/taffybar/issues/308)

- [CSS in GTK](https://docs.gtk.org/gtk3/css-overview.html)
- [GTK CSS Properties](https://docs.gtk.org/gtk3/css-properties.html)

Run Taffybar with the environment variable `GTK_DEBUG=interactive` to
enable the [GTK Inspector][inspector]. This will let you figure out
CSS class names of widgets.

[inspector]: https://developer.gnome.org/documentation/tools/inspector.html
