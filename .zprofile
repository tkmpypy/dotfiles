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
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.anyenv/bin:$PATH"

export PATH="$HOME/.anyenv/envs/nodenv/bin:$PATH"
export PATH="$HOME/.anyenv/envs/nodenv/versions/*/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="$PATH":"$HOME/.pub-cache/bin"

