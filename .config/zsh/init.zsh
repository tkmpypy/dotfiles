[ -f ~/.zshrc.local ] && source ~/.zshrc.local

if [ "$(uname -m)" = "arm64" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
else
  eval "$(/usr/local/bin/brew shellenv)"
fi

eval "$(mise activate zsh)"
eval "$(direnv hook zsh)"

