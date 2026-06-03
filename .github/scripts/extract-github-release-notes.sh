#!/usr/bin/env bash
set -euo pipefail

package="${1:?usage: extract-github-release-notes.sh <package> <version> <package-dir>}"
version="${2:?usage: extract-github-release-notes.sh <package> <version> <package-dir>}"
package_dir="${3:?usage: extract-github-release-notes.sh <package> <version> <package-dir>}"

if [[ "$package" == "taffybar" ]]; then
  changelog="CHANGELOG.md"
else
  changelog="$package_dir/CHANGELOG.md"
fi

if [[ -f "$changelog" ]]; then
  notes="$(
    awk -v version="$version" '
      /^#+[[:space:]]+/ {
        heading_level = match($0, /[^#]/) - 1
      }

      /^#+[[:space:]]+/ && $0 ~ "^#+[[:space:]]+" version "([[:space:]-]|$)" {
        section_level = heading_level
        in_section = 1
        found = 1
        next
      }

      in_section && /^#+[[:space:]]+/ && heading_level <= section_level { exit }
      in_section { print }
      END { if (!found) exit 1 }
    ' "$changelog" | sed '/./,$!d'
  )"

  if [[ -n "$notes" ]]; then
    printf '%s\n' "$notes"
    exit 0
  fi
fi

cat <<EOF
Released \`$package\` $version to Hackage.

- Hackage: https://hackage.haskell.org/package/$package-$version
EOF
