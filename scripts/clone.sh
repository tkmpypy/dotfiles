#!/bin/zsh

set -e

sh -c "$(curl -fsSL https://git.io/zinit-install)"

# install tpm
if [[ ! -d $HOME/.tmux/plugins/tpm ]]; then
    git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
fi

