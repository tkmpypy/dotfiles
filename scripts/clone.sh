#!/bin/bash

set -e

if [[ ! -d $HOME/.tmux/plugins/tpm ]]; then
    git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
fi

if [[ ! -d $HOME/.anyenv ]]; then
    git clone https://github.com/anyenv/anyenv $HOME/.anyenv
fi

