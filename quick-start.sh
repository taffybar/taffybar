#!/usr/bin/env bash

REPO_URI="${1-https://github.com/travitch/taffybar}"
TAFFY_CONFIG_DIR="$HOME/.config/taffybar/"

ensure_in_taffybar_repo () {
	if [ ! -f taffybar.cabal ]; then
		git clone "$REPO_URI"
		cd taffybar || exit
	fi
	REPO_LOCATION="$(pwd)"
}

# Check whether the given path is listed in the PATH environment variable
on_path () {
	echo ":$PATH:" | grep -q :"$1":
}

ensure_stack_present () {
	# Check whether ~/.local/bin is on the PATH, and put it there if it isn't
	if ! on_path "$HOME/.local/bin" ; then
		export PATH="$HOME/.local/bin:$PATH"
	fi

	# Get stack
	curl -sSL https://get.haskellstack.org/ | sh
}

setup_taffy_config () {
	mkdir -p "$TAFFY_CONFIG_DIR"
	cp -n my-taffybar.cabal.example "$TAFFY_CONFIG_DIR/my-taffybar.cabal"
	cp -n taffybar.hs.example "$TAFFY_CONFIG_DIR/taffybar.hs"
	cp -n stack.yaml.example "$TAFFY_CONFIG_DIR/stack.yaml"
	cd "$TAFFY_CONFIG_DIR" || exit
	cp -r "$REPO_LOCATION" .
	stack install --install-ghc
}

ensure_in_taffybar_repo
TAFFYBAR_SHA="$(git rev-parse HEAD)"
TAFFYBAR_REPO="$(git remote -v | head -n 1 | awk '{print $2}')"
ensure_stack_present
stack install gtk2hs-buildtools --install-ghc
setup_taffy_config
