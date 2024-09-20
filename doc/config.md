# Configuration (and compilation)

[dyre]: https://github.com/willdonnelly/dyre

There are broadly three ways of building your configuration:

1. [Install][] Taffybar, use the default configuration and don't compile anything yourself.
2. Let [Dyre][] handle compilation of `taffybar.hs`.
3. Compile your Taffybar executable some other way.

## Before installing

Taffybar's installation procedure varies depending your chosen option.

It is important for you to read this section so you can understand
what all of that means before you decide how you want to install
Taffybar.

## Comparison

|                          | **Dyre** | **Other**     |
| ------------------------ | -------- | ------------- |
| Config file location     | `$XDG_CONFIG_HOME/taffybar/taffybar.hs` | Anywhere |
| Automatic reloading      | Yes      | No  |
| Multiple modules         | No       | Yes |
| Main function            | [`dyreTaffybar`][dyreTaffybar] | [`startTaffybar`][startTaffybar] |
| Installation environment | Global   | Project-local |

[dyreTaffybar]: https://hackage.haskell.org/package/taffybar-4.0.2/docs/System-Taffybar.html#v:dyreTaffybar
[startTaffybar]: https://hackage.haskell.org/package/taffybar-4.0.2/docs/System-Taffybar.html#v:startTaffybar

## Dyre: `dyreTaffybar`

Dyre simply calls `ghc` directly to compile the config file. Any
Haskell packages used by the config (e.g. `taffybar`,
`xmonad-contrib`) must be installed and available to
`ghc-pkg`. Usually this means with a global Haskell installation.

The [`dyreTaffybar`][dyreTaffybar] entry point to Taffybar uses the
[Dyre library][dyre] to automatically recompile your
configuration whenever it detects that it has changed.

The binary that is distributed with Taffybar does nothing more than call this entry point.

The main downside of this approach is that it does not allow the user to use any
sort of project files for their configuration, and they must have any packages
that are necessary for compilation of their configuration available in their
global ghc environment.

## Not Dyre: `startTaffybar` 

Not using Dyre, you can compile your Taffybar configuration the same
way you compile other Haskell projects that you work on.

The user binary will not be automatically recompiled
when source files change.

The [`startTaffybar`][startTaffybar] entry point simply starts
Taffybar with the given configuration.

The advantage of using `startTaffybar` directly is
that you can use that in the `main` function of a Cabal project.

### The project approach

The project approach to installing/using taffybar involves maintaining a small
haskell project that produces the users taffybar binary. No matter which package
manager you choose to use you will need to make a `.cabal` file for this project.

It is recommended that you use [this
example](https://github.com/taffybar/taffybar/blob/master/example/my-taffybar.cabal)
as a template. In that example, the users configuration resides in the file
`taffybar.hs` in the same directory, but that can be changed as needed. 

#### Cabal

Simply run `cabal install` to install the binary.

#### Stack

With stack, you will also need to maintain a `stack.yaml` file. Run `stack
install` to install the binary.
See [this example](https://github.com/taffybar/taffybar/blob/master/example/stack.yaml)

#### Nix

You will need to add `default.nix` file to your package.
See [this example](https://github.com/taffybar/taffybar/blob/master/example/default.nix)

