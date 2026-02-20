# Configuration (and compilation)

[dyre]: https://github.com/willdonnelly/dyre

There are broadly three ways of building your configuration:

1. [Install](./install.md) Taffybar, use the default configuration (i.e. absent
   `taffybar.hs`) and don't compile anything yourself.
2. Let [Dyre][] handle compilation of `taffybar.hs`.
3. Compile your Taffybar executable some other way. Typically this
   would be done with the "Project Approach" (see below).

The `taffybar` binary also supports explicit startup subcommands:

- `taffybar auto` (default behavior when no subcommand is provided)
- `taffybar dyre`
- `taffybar example`
- `taffybar dhall`

See [`doc/dhall-config.md`](./dhall-config.md) for the experimental Dhall schema.

## Before installing

Taffybar's [installation procedure](./install.md) varies depending
your chosen option.

For example, it's not always necessary to install the `taffybar`
executable globally using a distro package or `cabal install
taffybar`.

Read this section so you can understand what all of that means before
you decide how you want to install Taffybar.

## Comparison

|                          | **Dyre** | **Not Dyre**  |
| ------------------------ | -------- | ------------- |
| Config file location     | `$XDG_CONFIG_HOME/taffybar/taffybar.hs` | Anywhere |
| Automatic reloading      | Yes      | No   |
| Multiple modules         | No       | Yes  |
| Boilerplate code/config  | Little   | Some |
| Entry point              | [`dyreTaffybar`][dyreTaffybar] | [`startTaffybar`][startTaffybar] |
| Installation environment | Global   | Project-local |

[dyreTaffybar]: https://hackage.haskell.org/package/taffybar-4.0.2/docs/System-Taffybar.html#v:dyreTaffybar
[startTaffybar]: https://hackage.haskell.org/package/taffybar-4.0.2/docs/System-Taffybar.html#v:startTaffybar

## Entry point

### `dyreTaffybar` (Dyre)

Dyre simply calls `ghc` directly to compile the config file. Any
Haskell packages used by the config (e.g. `taffybar`,
`xmonad-contrib`) must be installed and available to
`ghc-pkg`. Usually this means with a global Haskell installation.

The [`dyreTaffybar`][dyreTaffybar] entry point to Taffybar uses the
[Dyre library][dyre] to automatically recompile your
configuration whenever it detects that it has changed.

The binary that is distributed with Taffybar does nothing more than
call this entry point.

The main downside of this approach is that it does not allow the user to use any
sort of project files for their configuration, and they must have any packages
that are necessary for compilation of their configuration available in their
global ghc environment.

### `startTaffybar` (not Dyre)

Not using Dyre, you can compile your Taffybar configuration the same
way you compile other Haskell projects that you work on.

The user binary will _not_ be automatically recompiled when source
files change.

The [`startTaffybar`][startTaffybar] entry point simply starts
Taffybar with the given configuration.

The advantage of using `startTaffybar` directly is
that you can use that in the `main` function of a Cabal project.

## The Project Approach

The "project approach" to configuring and installing Taffybar involves
maintaining a small Haskell project that produces the `taffybar`
binary.

With this approach, you can build your Taffybar using proper build
tools rather than just `ghc -o taffybar taffybar.hs ...`, which is
essentially what [Dyre][] uses. The "Main" disadvantage of Dyre is
that you can't easily split your configuration into multiple modules.

It is recommended that you use [this example `my-taffybar.cabal`][example-cabal]
as a template. In that example, the user's configuration resides in the file
`taffybar.hs` within the same directory, but that can be changed as needed. 

[example-cabal]: https://github.com/taffybar/taffybar/blob/master/example/my-taffybar.cabal

### Main

Example: [Main](https://github.com/taffybar/taffybar/blob/master/example/taffybar.hs)

The `Main` module and any other module(s) listed in the `.cabal` file
are therefore your configuration.

Your `main` function needs to call the `startTaffybar` entrypoint with
a `TaffybarConfig` value. See [Customization](./custom.md) for further
information.

### Build tool examples

Use your chosen build tool to compile the `taffybar` executable of
your configuration. The build tool can then install `taffybar` to a
system location such as `/usr/local/bin`.

#### Cabal

Example: [`my-taffybar.cabal`][example-cabal]

Run `cabal install` within the project to install the executable.

#### Stack

Example: [`stack.yaml`](https://github.com/taffybar/taffybar/blob/master/example/stack.yaml)

With Stack, you will also need to maintain a `stack.yaml` file in
addition to the `.cabal` file. When choosing a "resolver" for
`stack.yaml`, the latest [LTS Haskell](https://www.stackage.org/lts)
is usually a good choice.

Run `stack install` within the project to install the executable.

#### Nix

Example derivation: [`default.nix`](https://github.com/taffybar/taffybar/blob/master/example/default.nix).

This could be installed into your Nix user environment with `nix-env -i -f default.nix`.
