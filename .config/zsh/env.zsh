
if [[ -z "$XDG_CONFIG_HOME" ]]
then
    export XDG_CONFIG_HOME="$HOME/.config/"
fi
if [[ -z "$XDG_CACHE_HOME" ]]
then
    export XDG_CACHE_HOME="$HOME/.cache/"
fi
if [[ -z "$XDG_DATA_HOME" ]]
then
    export XDG_DATA_HOME="$HOME/.local/share/"
fi
# export TERM="xterm-256color-italic"
export TERM='wezterm'
export EDITOR='nvim'
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.local/nvim/bin"
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:$HOME/.local/share/nvim/mason/bin"
export PATH="$PATH:$HOME/.local/share/rtx"
export AQUA_ROOT_DIR="$XDG_DATA_HOME/aquaproj-aqua"
export PATH="$PATH:$AQUA_ROOT_DIR/bin"
export AQUA_GLOBAL_CONFIG="$XDG_CONFIG_HOME/aqua/aqua.yaml"
export PATH="$PATH:$HOME/.docker/bin"
export GHCUP_USE_XDG_DIRS="true"

export PATH="$PATH":"$HOME/.pub-cache/bin"
