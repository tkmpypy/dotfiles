#!/bin/zsh

set -e

DOTFILES_DIR=$(
  cd $(dirname $0)/../
  pwd
)

OS=$(uname)
ARCH=$(uname -m)

function exec_cmd() {
  echo $1
  eval $1
}

function link_file() {
  dir_name=$(dirname $2)
  if [ $dir_name = "." ]; then
    exec_cmd "ln -sf $1/$2 $3"
  else
    exec_cmd "ln -sf $1/$2 $3/$dir_name"
  fi
}

function clone_dep() {
  exec_cmd "$DOTFILES_DIR/scripts/clone.sh"
}

function run_os_scripts() {
  exec_cmd "$DOTFILES_DIR/scripts/$OS/system.sh"
  exec_cmd "$DOTFILES_DIR/scripts/$OS/brew.sh"
}

function install() {
  exec_cmd "mkdir -p $HOME/.tmux"

  DOT_FILES=(.zshrc .zprofile .zshrc.$OS.$ARCH .vimrc .tmux.conf .ideavimrc .p10k.zsh .terminfo .vsnip)
  for file in ${DOT_FILES[@]}; do
    link_file $DOTFILES_DIR $file $HOME
  done

  if [[ -n "$XDG_CONFIG_HOME" ]]; then
    exec_cmd "mkdir -p $XDG_CONFIG_HOME"
    CONFIG_FILES=(nvim wezterm starship.toml k9s rtx helix)
    for file in ${CONFIG_FILES[@]}; do
      link_file $DOTFILES_DIR/.config $file $XDG_CONFIG_HOME
    done
  fi

  link_file $DOTFILES_DIR bin $HOME
}

function main() {
  install
  clone_dep
  run_os_scripts
}

main
