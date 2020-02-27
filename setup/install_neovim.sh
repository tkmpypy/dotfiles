nvim -v

cd ~/dotfiles/setup
rm -rf ./nvim-osx64/
curl -L https://github.com/neovim/neovim/releases/download/nightly/nvim-macos.tar.gz | tar zx

cd nvim-osx64

rm -f /usr/local/bin/nvim
ln -s  ~/dotfiles/setup/nvim-osx64/bin/nvim /usr/local/bin/

nvim -v
