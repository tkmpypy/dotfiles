DSTDIR=~/.local/share/nvim/lsp

function make_dir() {
    if [ ! -d $1 ]; then
      # 存在しない場合は作成（本処理へ)
      mkdir $1
    else
      # 存在して空でない場合は警告を出して終了
      if [ -n "$(ls -A $1)" ]; then
        echo "出力先にファイルが存在します。削除してください。=> $DSTDIR"
        exit 1
      fi
    fi
}

make_dir $DSTDIR

# rust-analyzer
rm -f ~/.local/bin/rust-analyzer
curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-mac -o ~/.local/bin/rust-analyzer
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

# for Mac
cd 3rd/luamake
ninja -f ninja/macos.ninja
cd ../..
./3rd/luamake/luamake rebuild
# ./bin/macOS/lua-language-server -E ./main.lua

# pyright
npm install -g pyright

# terraform-ls
