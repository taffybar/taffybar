#!/usr/bin/env bash
# Run a Nix command, retrying only when a build was killed by max-silent-time.
# Haddock occasionally hangs forever on CI; completed builds stay in the store,
# so a retry resumes from where the hung build left off.
set -uo pipefail

attempts="${NIX_HANG_RETRIES:-3}"
log="$(mktemp)"
trap 'rm -f "$log"' EXIT

for ((attempt = 1; attempt <= attempts; attempt++)); do
  "$@" 2>&1 | tee "$log"
  status="${PIPESTATUS[0]}"
  if [[ "$status" -eq 0 ]]; then
    exit 0
  fi
  if ! grep -q "seconds of silence" "$log"; then
    exit "$status"
  fi
  echo "::warning::A Nix build hung without output (attempt $attempt/$attempts)."
done

exit "$status"
