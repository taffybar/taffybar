#!/usr/bin/env bash
set -euo pipefail

tag="${1:?usage: resolve-hackage-release.sh <tag>}"

if [[ "$tag" =~ ^v([0-9]+(\.[0-9]+)+)$ ]]; then
  package="taffybar"
  version="${BASH_REMATCH[1]}"
elif [[ "$tag" =~ ^([A-Za-z0-9_-]+)-v([0-9]+(\.[0-9]+)+)$ ]]; then
  package="${BASH_REMATCH[1]}"
  version="${BASH_REMATCH[2]}"
else
  echo "Unsupported release tag: $tag" >&2
  exit 1
fi

case "$package" in
  taffybar)
    package_dir="."
    cabal_file="taffybar.cabal"
    ;;
  dbus-hslogger|dbus-menu|gi-wireplumber|gtk-scaling-image|gtk-sni-tray|gtk-strut|status-notifier-item|xdg-desktop-entry)
    package_dir="packages/$package"
    cabal_file="$package_dir/$package.cabal"
    ;;
  *)
    echo "No Hackage package mapping for tag '$tag'." >&2
    exit 1
    ;;
esac

actual_name="$(awk -F: 'tolower($1) == "name" { gsub(/^[ \t]+|[ \t]+$/, "", $2); print $2; exit }' "$cabal_file")"
actual_version="$(awk -F: 'tolower($1) == "version" { gsub(/^[ \t]+|[ \t]+$/, "", $2); print $2; exit }' "$cabal_file")"

if [[ "$actual_name" != "$package" ]]; then
  echo "Tag selected package '$package', but $cabal_file declares '$actual_name'." >&2
  exit 1
fi

if [[ "$actual_version" != "$version" ]]; then
  echo "Tag selected version '$version', but $cabal_file declares '$actual_version'." >&2
  exit 1
fi

cat <<EOF
tag=$tag
package=$package
version=$version
package_dir=$package_dir
cabal_file=$cabal_file
sdist=dist-sdist/$package-$version.tar.gz
EOF
