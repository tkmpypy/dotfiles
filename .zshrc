# function source {
#   ensure_zcompiled $1
#   builtin source $1
# }
# function ensure_zcompiled {
#   local compiled="$1.zwc"
#   if [[ ! -r "$compiled" || "$1" -nt "$compiled" ]]; then
#     echo "\033[1;36mCompiling\033[m $1"
#     zcompile $1
#   fi
# }
# ensure_zcompiled ~/.zshrc

# # ref: https://zenn.dev/fuzmare/articles/zsh-plugin-manager-cache
# cache_dir=${XDG_CACHE_HOME:-$HOME/.cache}
# sheldon_cache="${cache_dir}/sheldon.zsh"
# sheldon_toml="${XDG_CONFIG_HOME:-$HOME/.config}sheldon/plugins.toml"
# # キャッシュがない、またはキャッシュが古い場合にキャッシュを作成
# if [[ ! -r "$sheldon_cache" || "$sheldon_toml" -nt "$sheldon_cache" ]]; then
#   mkdir -p $cache_dir
#   sheldon source > $sheldon_cache
# fi
# source "$sheldon_cache"

# unset cache_dir sheldon_cache sheldon_toml
# zsh-defer unfunction source

eval "$(sheldon source)"
