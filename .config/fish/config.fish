# encoding
set -x LANG ja_JP.UTF-8

#set fisher_home ~/.local/share/
#set fisher_config ~/.config/fisherman
#source $fisher_home/config.fish

set -x XDG_CONFIG_HOME $HOME/.config

set -x PATH ~/.local/bin $PATH

if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

set -x PATH ~/.nimble/bin $PATH

set -x PATH /Users/takuma/.nimble/bin $PATH

# set -x PATH $HOME/.nodebrew/current/bin $PATH

# set -x JAVA_HOME (/usr/libexec/java_home -v 1.8)

set -x PATH /usr/local/bin $PATH
set -x PATH /usr/local/opt/node@6/bin $PATH

# Android
# set -x ANDROID_HOME $HOME/Library/Android/sdk
# set -x PATH $ANDROID_HOME/platform-tools $PATH

# Dart & Flutter
set -x PATH ~/.pub-cache/bin $PATH
set -x PATH /Volumes/Samsung_T5/flutter-sdk/flutter/bin $PATH
set -x PATH /Volumes/Samsung_T5/flutter-sdk/flutter/.pub-cache/bin $PATH
# set -x PATH ~/flutter-sdk/flutter/bin $PATH
# set -x PATH ~/flutter-sdk/flutter/.pub-cache/bin $PATH

# Python
set -x PYENV_ROOT $HOME/.pyenv/
set -x PATH $PYENV_ROOT/bin $PATH
. (pyenv init -|psub)

# Go
set -x GOENV_ROOT $HOME/.goenv
set -x PATH $GOENV_ROOT/.goenv/bin $PATH
status --is-interactive; and source (goenv init -|psub)
set -x PATH $GOROOT/.goenv/bin $PATH
set -x PATH $GOPATH/.goenv/bin $PATH
set -x GOPATH $HOME/go
set -x PATH $GOPATH/bin $PATH

# Rust
set -x PATH $HOME/.cargo/bin $PATH

# Node.js
set -x PATH $HOME/.nodebrew/current/bin $PATH

set -x PATH /opt/local/bin:/opt/local/sbin $PATH
set -x MANPATH /opt/local/man $MANPATH
# set -x PATH $PATH


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/takuma/google-cloud-sdk/path.fish.inc' ]; . '/Users/takuma/google-cloud-sdk/path.fish.inc'; end
