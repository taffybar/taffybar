# Customization

For details about the library modules for configuring your Taffybar,
see the [full documentation][hackage].

[hackage]: https://hackage.haskell.org/package/taffybar
[gi-gtk]: https://hackage.haskell.org/package/gi-gtk

- [`taffybar` package documentation][hackage] - `System.Taffybar`
- [`gi-gtk` package documentation][gi-gtk] - `GI.Gtk`

## Config file location

Taffybar uses the directory `$XDG_CONFIG_HOME/taffybar/`. As per the
[XDG Base Directory Specification][basedir-spec], an unset or empty
`XDG_CONFIG_HOME` environment variable is taken to mean `~/.config`.

[basedir-spec]: https://specifications.freedesktop.org/basedir-spec/latest/#variables

## `TaffybarConfig`

Be aware that the `TaffybarConfig` value required by `dyreTaffybar`/`startTaffybar` is normally constructed via a [`SimpleConfig`](https://hackage.haskell.org/package/taffybar/docs/System-Taffybar-SimpleConfig.html#t:SimpleTaffyConfig) value and [`toTaffybarConfig`](https://hackage.haskell.org/package/taffybar/docs/System-Taffybar-SimpleConfig.html#v:toTaffybarConfig).

## CSS

[#308 Add styling tips section to README/docs](https://github.com/taffybar/taffybar/issues/308)

Appearance of Taffybar widgets can be controlled with CSS rules. These
are by default loaded from `$XDG_CONFIG_HOME/taffybar/taffybar.css`. Taffybar
must be restarted for changes in `taffybar.css` to take effect.

### GTK Documentation

CSS styling is a feature of GTK. It uses a limited version of CSS, so
the following articles from the GTK documentation are useful:
- [CSS in GTK](https://docs.gtk.org/gtk3/css-overview.html)
- [GTK CSS Properties](https://docs.gtk.org/gtk3/css-properties.html)

### GTK Inspector

Run Taffybar with the environment variable `GTK_DEBUG=interactive` to
enable the [GTK Inspector][inspector]. This will let you figure out
CSS class names of widgets. The GTK Inspector also lets you
interactively try CSS rules, which is immensely helpful.

[inspector]: https://developer.gnome.org/documentation/tools/inspector.html

### Specifying colours

Note that the first three parameters of `rgba()` are integers in the
range 0—255, but the fourth is a float in the range 0.0—1.0. For example:

```css
.taffy-box {
  background-color: rgba(255, 250, 205, 0.3);
}
```
