# dotfiles

## Installation

### Enable undercurl

> You may wish to try these steps to install a copy of a wezterm terminfo file; this will compile a copy of the terminfo and install it into your ~/.terminfo directory:

```sh
tempfile=$(mktemp) \
  && curl -o $tempfile https://raw.githubusercontent.com/wez/wezterm/master/termwiz/data/wezterm.terminfo \
  && tic -x -o ~/.terminfo $tempfile \
  && rm $tempfile
```

### Clone this repo

```sh
git clone https://github.com/tkmpypy/dotfiles.git ~/ghq/github.com/tkmpypy/dotfiles
```

### Run scripts

```sh
$ cd ~/ghq/github.com/tkmpypy/dotfiles
$ ./scripts/install.sh
```

## Herdr plugins

必要な Herdr プラグインは以下です。Full 版は端末上の選択テキストへの注釈に加えて、Markdown とエージェント応答のレビューを提供します。

| プラグイン | 導入コマンド |
| --- | --- |
| [plannotator/herdr-annotate](https://github.com/plannotator/herdr-annotate) (Full) | `herdr plugin install plannotator/herdr-annotate` |

キーバインドの prefix は標準の `Ctrl+b` です。

| キー | 動作 |
| --- | --- |
| `prefix+a` | 選択した端末テキストに注釈を付ける |
| `prefix+shift+a` | 注釈をコンテキストとしてコピー |
| `prefix+ctrl+a` | 注釈をコピーしてアーカイブ |
| `prefix+ctrl+v` | 注釈をエージェントのプロンプトへ貼り付けてアーカイブ |
| `prefix+ctrl+s` | 注釈をエージェントへ送信してアーカイブ |
| `prefix+m` | 注釈を管理 |
| `prefix+o` | 現在のフォルダの文書をレビュー |
| `prefix+shift+o` | エージェントの直近の応答をレビュー |
| `prefix+ctrl+o` | エージェントの最新応答を直接レビュー |

## Herdr workspace pickers

Herdr 内で、次のキーから fzf の popup を開けます。prefix は標準の `Ctrl+b` です。

| キー | 動作 |
| --- | --- |
| `prefix+f` | 現在以外の workspace を選んで移動 |
| `prefix+g` | ghq のリポジトリを選び、workspace を再利用または作成 |
| `prefix+alt+g` | Herdr 標準の Goto picker |

workspace の選択画面には、アクティブな tab の pane の作業ディレクトリと最近の端末出力を表示します。
ghq の選択画面には README のプレビューを表示します。どちらも `Ctrl+d` / `Ctrl+u` でプレビューをスクロールできます。
同じリポジトリに対応する workspace が 1 件なら即移動、複数なら fzf で選択します。
Git worktree は別の作業ディレクトリとして扱います。`Esc` / `Ctrl+c` でキャンセルすると何も変更しません。

Herdr 内のシェルからも `herdr-switch-workspace` / `herdr-switch-workspace-from-ghq` を実行できます。
Herdr 外からの起動・接続には対応していません。

必要なコマンド: `herdr`（0.9.0 で確認）、`fzf`、`jq`、`ghq`、`git`、`shasum`。
`bat` があれば README をカラー表示します。
選択結果の形式を一定に保つため、この 2 つの picker では `FZF_DEFAULT_OPTS` と `FZF_DEFAULT_OPTS_FILE` を使用しません。
インストールスクリプトが作成する `~/bin` と `~/.config/herdr/config.toml` のリンク経由で利用します。
設定を変更した後は Herdr 内で `herdr server reload-config` を実行してください。
