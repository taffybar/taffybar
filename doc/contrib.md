# Contributing

<!-- NB. This file is linked to when opening pull requests. -->

If you want to help, but don't know where to get started you can check
out these issue labels:

- [Help Wanted](https://github.com/taffybar/taffybar/labels/help%20wanted)
- [Easy](https://github.com/taffybar/taffybar/labels/easy)

## Building local WirePlumber bindings

The default `cabal.project` leaves `gi-wireplumber` out of the local package
list so that disabling Taffybar's `wireplumber` flag also removes the need for
WirePlumber development libraries. When the flag is enabled, Cabal uses the
published bindings from Hackage.

To develop against the bindings in `packages/gi-wireplumber`, use the development
project (requires Cabal 3.8 or newer and the WirePlumber development libraries):

```sh
cabal build all --project-file=cabal.project.dev
```

The Nix flake always uses the local bindings.

