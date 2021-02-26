#!/bin/bash

DSTDIR=~/.local/share/nvim/lsp

function make_dir() {
    if [ ! -d $1 ]; then
      # 存在しない場合は作成（本処理へ)
      mkdir $1
    fi
}

make_dir $DSTDIR

# rust-analyzer
rm -f ~/.local/bin/rust-analyzer
if [ "$(uname)" == 'Darwin' ]; then
  curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-mac -o ~/.local/bin/rust-analyzer
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
  curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/.local/bin/rust-analyzer
fi
chmod +x ~/.local/bin/rust-analyzer

# vim-language-server
npm install -g vim-language-server

# diagnostic-languageserver
npm install -g diagnostic-languageserver

# lua-language-server
cd $DSTDIR
make_dir ./lua
cd ./lua
# clone project
git clone https://github.com/sumneko/lua-language-server
cd lua-language-server
git submodule update --init --recursive

cd 3rd/luamake
if [ "$(uname)" == 'Darwin' ]; then
  ninja -f ninja/macos.ninja
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
  ninja -f ninja/linux.ninja
fi
cd ../..
./3rd/luamake/luamake rebuild
# ./bin/macOS/lua-language-server -E ./main.lua

# pyright
npm install -g pyright

# yaml-language-server
npm install -g yaml-language-server

# bash-language-server
npm install -g bash-language-server

# Docker
npm install -g dockerfile-language-server-nodejs
