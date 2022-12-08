ulimit -n 20480

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
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/local/bin"

export PATH="$PATH":"$HOME/.pub-cache/bin"

eval "$(direnv hook zsh)"

if [ "$(uname -m)" = "arm64" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
  export PATH="/opt/homebrew/bin:$PATH"
else
  eval "$(/usr/local/bin/brew shellenv)"
fi
