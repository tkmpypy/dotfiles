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

set -x GOROOT /usr/local/Cellar/go/1.8/libexec
set -x GOPATH $HOME/Go
set -x PATH $GOROOT/bin $PATH
set -x PATH $GOPATH/bin $PATH
set -x PATH ~/.nimble/bin $PATH

set -x PATH /Users/takuma/.nimble/bin $PATH

# flutter
set -x PATH $HOME/flutter-sdk/flutter/bin $PATH

# set -x PATH $HOME/.nodebrew/current/bin $PATH

# set -x JAVA_HOME (/usr/libexec/java_home -v 1.8)

set -x PATH $HOME/android-sdk $PATH

set -x PATH /usr/local/bin $PATH
set -x PATH /usr/local/opt/node@6/bin $PATH

set -x ANDROID_HOME $HOME/android-sdk
set -x PATH $ANDROID_HOME/tools $PATH

set -x PATH ~/flutter-sdk/flutter/bin $PATH

set -x PATH $HOME/.pyenv/ $PATH
. (pyenv init -|psub)

set -x PATH $HOME/.pyenv/versions/3.5.1/bin $PATH

# set -x PATH $PATH

alias emacs=/usr/local/bin/emacs
