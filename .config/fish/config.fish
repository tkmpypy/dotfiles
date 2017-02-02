# encoding
set -x LANG ja_JP.UTF-8

set fisher_home ~/.local/share/fisherman
set fisher_config ~/.config/fisherman
source $fisher_home/config.fish

set -x XDG_CONFIG_HOME $HOME/.config

set -x GOROOT /usr/local/Cellar/go/1.7.1/libexec
set -x GOPATH $HOME/Go
set -x PATH $GOROOT/bin $PATH
set -x PATH $GOPATH/bin $PATH

set -x PATH $HOME/.nodebrew/current/bin $PATH

set -x JAVA_HOME (/usr/libexec/java_home -v 1.8)

set -x PATH $HOME/.pyenv/ $PATH
. (pyenv init -|psub)

# set -x PATH $PATH
