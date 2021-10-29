# Env
if [[ -z "$XDG_CONFIG_HOME" ]]
then
    export XDG_CONFIG_HOME="$HOME/.config/"
fi
if [[ -z "$XDG_CACHE_HOME" ]]
then
    export XDG_CACHE_HOME="$HOME/.cache/"
fi
# export TERM="xterm-256color-italic"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/local/bin"
export PATH="$HOME/.cargo/bin:$PATH"

export PATH="$GOPATH/bin:$PATH"
export PATH="$PATH":"$HOME/.pub-cache/bin"

export NVIM_LOG_FILE="$XDG_CACHE_HOME/nvim/.nvim.log"

