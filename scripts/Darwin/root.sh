#!/usr/bin/env zsh

set -e

function exec_cmd() {
  echo $1
  eval $1
}

exec_cmd "$DOTFILES_DIR/scripts/$OS/clone.sh"
exec_cmd "$DOTFILES_DIR/scripts/$OS/system.sh"
exec_cmd "$DOTFILES_DIR/scripts/$OS/brew.sh"
