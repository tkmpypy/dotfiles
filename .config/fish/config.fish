export XDG_CONFIG_HOME="$HOME/.config/"
export XDG_CACHE_HOME="$HOME/.cache/"
export XDG_DATA_HOME="$HOME/.local/share/"
# export TERM="xterm-256color-italic"
# export TERM='wezterm'
export EDITOR='nvim'
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.local/nvim/bin"
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:$HOME/.local/share/nvim/mason/bin"
export PATH="$PATH:$HOME/.local/share/mise/"
export PATH="/opt/homebrew/bin:$PATH"
export AQUA_ROOT_DIR="$XDG_DATA_HOME/aquaproj-aqua"
export PATH="$PATH:$AQUA_ROOT_DIR/bin"
export AQUA_GLOBAL_CONFIG="$XDG_CONFIG_HOME/aqua/aqua.yaml"
export PATH="$PATH:$HOME/.docker/bin"
export GHCUP_USE_XDG_DIRS="true"
export BOB_CONFIG="$XDG_CONFIG_HOME/bob/config.json"

export PATH="$PATH":"$HOME/.pub-cache/bin"

set local_config $HOME/.config.local.fish
if test -f $local_config
    source $local_config
end

set --erase fish_greeting

if status is-interactive
end

eval (direnv hook fish)
mise activate fish | source

abbr -a ll 'ls -la'
abbr -a gp 'git pull'
abbr -a gswc 'git switch -c'
abbr -a aqls 'aqua list | fzf'
abbr -a aqgi 'aqua g -i'
abbr -a aqia 'aqua i -l -a'
abbr -a vim 'nvim'
abbr -a k9s 'LC_CTYPE="en_US.UTF-8" k9s'
abbr -a ssh 'TERM=xterm-256color ssh'
abbr -a lnav 'TERM=xterm-256color lnav'

function update_completion
  set -l cmd ( type --force-path $argv[1] ); or return
  set -l out $HOME/.config/fish/completions/$argv[1].fish
  if not test $out -nt $cmd # is true when out is not found or older than cmd
    mkdir -p $HOME/.config/fish/completions
    $cmd $argv[2..-1] > $out
  end
end

update_completion gh completion -s fish
update_completion mise completion fish

