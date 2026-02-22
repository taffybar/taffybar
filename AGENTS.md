# Agent Guidelines

## Environment Loading

- Run project commands through `direnv` so the expected toolchain and env vars
  from `.envrc` are active.
- Use `direnv exec . <command>` for non-interactive commands (builds, tests,
  formatters, linters, etc.).

## Formatting Before Commit

- Always run `ormolu` on all changed Haskell files before every commit.
- Recommended command:

  ```bash
  { git diff --name-only -- '*.hs'; git ls-files --others --exclude-standard -- '*.hs'; } \
    | sort -u \
    | xargs -r ormolu --mode inplace
  ```

- After formatting, re-run build/tests and only then commit.
