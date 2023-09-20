# ref: https://zenn.dev/fuzmare/articles/zsh-plugin-manager-cache
cache_dir=${XDG_CACHE_HOME:-$HOME/.cache}
sheldon_cache="$cache_dir/sheldon.zsh"
sheldon_toml="${XDG_CONFIG_HOME:-$HOME/.config}sheldon/plugins.toml"
# キャッシュがない、またはキャッシュが古い場合にキャッシュを作成
if [[ ! -r "$sheldon_cache" || "$sheldon_toml" -nt "$sheldon_cache" ]]; then
  mkdir -p $cache_dir
  sheldon source > $sheldon_cache
fi
source "$sheldon_cache"
# 使い終わった変数を削除
unset cache_dir sheldon_cache sheldon_toml

