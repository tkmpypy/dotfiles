#!/usr/bin/env zsh

set -e

export XDG_CONFIG_HOME="$HOME/.config/"
export DOTFILES_DIR="~/ghq/github.com/tkmpypy/dotfiles"

export OS=$(uname)
export ARCH=$(uname -m)

function exec_cmd() {
  echo $1
  eval $1
}

function link_file() {
  dir_name=$(dirname $2)
  if [ $dir_name = "." ]; then
    exec_cmd "ln -sf $1/$2 $3"
  else
    mkdir -p $3/$dir_name
    exec_cmd "ln -sf $1/$2 $3/$dir_name"
  fi
}

function run_os_scripts() {
  exec_cmd "$DOTFILES_DIR/scripts/$OS/root.sh"
}

function link() {
  exec_cmd "mkdir -p $HOME/.tmux"

  DOT_FILES=(.zshrc .vimrc .tmux.conf .ideavimrc .p10k.zsh .vsnip)
  for file in ${DOT_FILES[@]}; do
    link_file $DOTFILES_DIR $file $HOME
  done

  exec_cmd "mkdir -p $XDG_CONFIG_HOME"
  CONFIG_FILES=(git nvim wezterm k9s/skin.yml mise helix aqua sheldon zsh fish bob)
  for file in ${CONFIG_FILES[@]}; do
    link_file $DOTFILES_DIR/.config $file $XDG_CONFIG_HOME
  done

  link_file $DOTFILES_DIR bin $HOME
}

function term() {
  unset DOTFILES_DIR
  unset OS
  unset ARCH
  unset XDG_CONFIG_HOME
}

function main() {
  run_os_scripts
  link
  term
}

main
