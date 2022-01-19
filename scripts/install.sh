#!/bin/bash

set -e

DOTFILES_DIR=$(
  cd $(dirname $0)/../
  pwd
)

function exec_cmd() {
  echo $1
  eval $1
}

function link_file() {
  dir_name=$(dirname $2)
  if [ $dir_name = "." ]; then
    exec_cmd "ln -s $1/$2 $3/"
  else
    exec_cmd "ln -s $1/$2 $3/$dir_name"
  fi
}

function clone_dep() {
  exec_cmd "$DOTFILES_DIR/scripts/clone.sh"
}

function run_os_scripts() {
  exec_cmd "$DOTFILES_DIR/scripts/$(uname)/system.sh"
  exec_cmd "$DOTFILES_DIR/scripts/$(uname)/brew.sh"
}

function install() {
  exec_cmd "mkdir -p $HOME/.tmux"

  DOT_FILES=(.zshrc .zprofile .zshrc.$(uname) .vimrc .tmux.conf .ideavimrc .p10k.zsh)
  for file in ${DOT_FILES[@]}; do
    link_file $DOTFILES_DIR $file $HOME
  done

  if [[ -n "$XDG_CONFIG_HOME" ]]; then
    exec_cmd "mkdir -p $XDG_CONFIG_HOME"
    CONFIG_FILES=(nvim starship.toml)
    for file in ${CONFIG_FILES[@]}; do
      link_file $DOTFILES_DIR/.config $file $XDG_CONFIG_HOME
    done
  fi
}

function main() {
  clone_dep
  run_os_scripts
  install
}

main
