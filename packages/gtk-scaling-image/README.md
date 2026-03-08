# gtk-scaling-image

`gtk-scaling-image` provides generic GTK image sizing helpers for Haskell GI
applications. It includes:

- aspect-ratio-preserving pixbuf scaling to a target size
- CSS-aware border and padding measurement helpers
- a draw-based auto-fill image widget that scales to allocation without
  `Gtk.Image` resize feedback loops

This package was extracted from the shared image widget code used by
`taffybar` and `gtk-sni-tray`.
