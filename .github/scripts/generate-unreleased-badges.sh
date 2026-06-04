#!/usr/bin/env bash
set -euo pipefail

out_dir="${1:?usage: generate-unreleased-badges.sh <output-dir> [git-ref]}"
git_ref="${2:-HEAD}"

mkdir -p "$out_dir"

packages=(
  taffybar
  dbus-hslogger
  dbus-menu
  gi-wireplumber
  gtk-scaling-image
  gtk-sni-tray
  gtk-strut
  status-notifier-item
  xdg-desktop-entry
)

badge_color() {
  local count="$1"

  if [[ "$count" == "0" ]]; then
    printf 'brightgreen'
  else
    printf 'blue'
  fi
}

write_badge() {
  local package="$1"
  local count="$2"
  local label
  local color

  if [[ "$package" == "taffybar" ]]; then
    label="unreleased commits"
  else
    label="$package unreleased"
  fi

  color="$(badge_color "$count")"

  cat > "$out_dir/$package.json" <<EOF
{
  "schemaVersion": 1,
  "label": "$label",
  "message": "$count",
  "color": "$color"
}
EOF
}

write_missing_badge() {
  local package="$1"

  cat > "$out_dir/$package.json" <<EOF
{
  "schemaVersion": 1,
  "label": "$package unreleased",
  "message": "missing tag",
  "color": "red"
}
EOF
}

for package in "${packages[@]}"; do
  if [[ "$package" == "taffybar" ]]; then
    tag_pattern="v*"
  else
    tag_pattern="$package-v*"
  fi

  tag="$(git tag --list "$tag_pattern" --sort=-v:refname | head -1)"

  if [[ -z "$tag" ]]; then
    write_missing_badge "$package"
    continue
  fi

  if [[ "$package" == "taffybar" ]]; then
    count="$(
      git rev-list --count "$tag..$git_ref" -- . ':(exclude)packages/*'
    )"
  else
    eval "$(.github/scripts/resolve-hackage-release.sh "$tag")"
    count="$(git rev-list --count "$tag..$git_ref" -- "$package_dir")"
  fi

  write_badge "$package" "$count"
done
