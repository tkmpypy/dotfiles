# dotfiles

## Installation

### Enable undercurl

> You may wish to try these steps to install a copy of a wezterm terminfo file; this will compile a copy of the terminfo and install it into your ~/.terminfo directory:

```sh
tempfile=$(mktemp) \
  && curl -o $tempfile https://raw.githubusercontent.com/wez/wezterm/master/termwiz/data/wezterm.terminfo \
  && tic -x -o ~/.terminfo $tempfile \
  && rm $tempfile
```

### Clone this repo

```sh
git clone https://github.com/tkmpypy/dotfiles.git ~/ghq/github.com/tkmpypy/dotfiles
```

### Run scripts

```sh
$ cd ~/ghq/github.com/tkmpypy/dotfiles
$ ./scripts/install.sh
```
