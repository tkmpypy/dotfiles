bindkey -e

bindkey '^xe' edit-command-line

# autoload -U history-search-end
# zle -N history-beginning-search-backward-end history-search-end
# zle -N history-beginning-search-forward-end history-search-end
# bindkey '^P' history-beginning-search-backward-end
# bindkey '^N' history-beginning-search-forward-end
bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search

# bindkey '^R' history-incremental-search-backward
# bindkey '^S' history-incremental-search-forward

bindkey '^A' beginning-of-line
bindkey '^E' end-of-line

bindkey '^r' fzf-select-history
