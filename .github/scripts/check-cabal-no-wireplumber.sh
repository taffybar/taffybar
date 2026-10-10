#!/usr/bin/env bash
set -euo pipefail

project_file="${1:-cabal.project}"
scratch_dir="$(mktemp -d)"
trap 'rm -rf "$scratch_dir"' EXIT

export REAL_PKG_CONFIG
REAL_PKG_CONFIG="$(command -v pkg-config)"
cat > "$scratch_dir/pkg-config" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
if [[ "$*" == *wireplumber* ]]; then
  exit 1
fi
"$REAL_PKG_CONFIG" "$@" | sed '/^wireplumber[- ]/d'
EOF
chmod +x "$scratch_dir/pkg-config"

check_plan() {
  local build_dir="$1"
  shift
  cabal install --dry-run --offline --project-file="$project_file" \
    --builddir="$build_dir" --with-pkg-config="$scratch_dir/pkg-config" "$@"

  python3 - "$build_dir/cache/plan.json" <<'PY'
import json
import sys

with open(sys.argv[1]) as plan_file:
    plan = json.load(plan_file)["install-plan"]

assert any(package["pkg-name"] == "taffybar" for package in plan)
assert not any(package["pkg-name"] == "gi-wireplumber" for package in plan)
PY
}

check_plan "$scratch_dir/unscoped" -f -wireplumber
check_plan "$scratch_dir/scoped" --constraint='taffybar -wireplumber'
