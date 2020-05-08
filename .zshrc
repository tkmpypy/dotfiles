source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Env
if [[ -z "$XDG_CONFIG_HOME" ]]
then
        export XDG_CONFIG_HOME="$HOME/.config/"
fi
export TERM="xterm-256color-italic"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/local/bin"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.anyenv/bin:$PATH"
if command -v anyenv 1>/dev/null 2>&1; then
  eval "$(anyenv init -)"
fi
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
export PATH="$HOME/.anyenv/envs/nodenv/bin:$PATH"
export PATH="$HOME/.anyenv/envs/nodenv/versions/*/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

# Two regular plugins loaded without tracking.
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-syntax-highlighting
zinit light zdharma/fast-syntax-highlighting

# Plugin history-search-multi-word loaded with tracking.
zinit load zdharma/history-search-multi-word

# A glance at the new for-syntax – load all of the above
# plugins with a single command. For more information see:
# https://zdharma.org/zinit/wiki/For-Syntax/
zinit for \
    light-mode  zsh-users/zsh-autosuggestions \
                zsh-users/zsh-syntax-highlighting \
    light-mode  zdharma/fast-syntax-highlighting \
                zdharma/history-search-multi-word \

# Handle completions without loading any plugin, see "clist" command.
# This one is to be ran just once, in interactive session.
# if [ ! -d ~/.zsh_completions ]; then
#     mkdir ~/.zsh_completions
# fi
# zinit creinstall ~/.zsh_completions

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma/zinit)…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi


# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-bin-gem-node

### End of Zinit's installer chunk

# Theme
zinit load denysdovhan/spaceship-prompt.git

# alias
alias ls='gls --color=auto'
alias ll='gls --color=auto -la'

# 文字コードの指定
export LANG=ja_JP.UTF-8

# 色を使用出来るようにする
autoload -Uz colors
colors

# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# cdなしでディレクトリ移動
setopt auto_cd

# ビープ音の停止
setopt no_beep

# ビープ音の停止(補完時)
setopt nolistbeep

# cd -<tab>で以前移動したディレクトリを表示
setopt auto_pushd

# ヒストリ(履歴)を保存、数を増やす
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 直前と同じコマンドの場合は履歴に追加しない
setopt hist_ignore_dups

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

setopt no_flow_control

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
### End of Zinit's installer chunk
### End of Zinit's installer chunk

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
