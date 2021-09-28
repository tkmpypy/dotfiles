#!/bin/bash

CURRENT=$(
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


exec_cmd "mkdir -p $XDG_CONFIG_HOME"
exec_cmd "mkdir -p $HOME/.tmux/tmuxline"

DOT_FILES=(.zshrc .zprofile .zshrc.$(uname) .vimrc .tmux.conf .tmux/tmuxline .emacs.d .ideavimrc)
for file in ${DOT_FILES[@]}; do
    link_file $CURRENT $file $HOME
done

CONFIG_FILES=(nvim starship.toml)
for file in ${CONFIG_FILES[@]}; do
    link_file $CURRENT/.config $file $XDG_CONFIG_HOME
done
