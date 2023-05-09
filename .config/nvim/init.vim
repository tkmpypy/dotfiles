scriptencoding=utf-8

lua if vim.loader then vim.loader.enable() end

let mapleader = "\<Space>"

let g:use_treesitter = v:true
let g:lsp_client_type = 'neovim' " neovim(builtin), coc
let g:git_client_type = 'neogit' " neogit, ??????
let g:file_explorer_type = 'neo-tree' " neo-tree, nvim-tree
let g:splash_type = 'dashboard-nvim' " dashboard-nvim, alpha-nvim

let g:did_install_default_menus = 1
let g:did_install_syntax_menu   = 1
let g:did_indent_on             = 1
let g:did_load_ftplugin         = 1
let g:loaded_2html_plugin       = 1
let g:loaded_gzip               = 1
let g:loaded_man                = 1
let g:loaded_matchit            = 1
let g:loaded_matchparen         = 1
let g:loaded_netrw              = 1
let g:loaded_netrwPlugin        = 1
let g:loaded_remote_plugins     = 1
let g:loaded_shada_plugin       = 1
let g:loaded_spellfile_plugin   = 1
let g:loaded_tarPlugin          = 1
let g:loaded_tutor_mode_plugin  = 1
let g:loaded_zipPlugin          = 1
let g:skip_loading_mswin        = 1
" let g:do_legacy_filetype=0

" You might have to force true color when using regular vim inside tmux as the
" colorscheme can appear to be grayscale with "termguicolors" option enabled.
if !has('gui_running') && &term =~ '^\%(screen\|tmux\)'
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
" let &t_Cs = "\e[4:3m"
" let &t_Ce = "\e[4:0m"

" Use as many color as possible
" if !has('gui_running')
"       \ && exists('&termguicolors')
"       \ && $COLORTERM =~# '^\%(truecolor\|24bit\)$'
"   " https://medium.com/@dubistkomisch/how-to-actually-get-italics-and-true-colour-to-work-in-iterm-tmux-vim-9ebe55ebc2be
" " use truecolor in term
"   if exists('&pumblend')
"     set pumblend=20
"   endif
" endif


set termguicolors
filetype plugin indent on
syntax on

set shell=zsh
set mouse=n

set laststatus=3
set background=dark
set splitkeep=screen

set clipboard^=unnamed,unnamedplus

set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8

au TextYankPost * silent! lua vim.highlight.on_yank {timeout = 300}

" completion settings
set complete&
    \ complete-=i
    \ complete-=t
set completeopt=menu,menuone,noselect

" set ttyfast
" set lazyredraw
set redrawtime=500
" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=500
set nowrap
set wildmenu
set wildmode=full
" set virtualedit=all
set number norelativenumber

set showmode " 現在のモードを表示
set showcmd " 打ったコマンドをステータスラインの下に表示
set noruler
set cursorline
set nocursorcolumn
set hlsearch
set backspace=indent,eol,start

if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif
set display+=lastline

set list
set listchars=tab:»\ ,trail:░,eol:↲,extends:☛,precedes:☚,nbsp:␣,conceal:┊

" No beep
set visualbell
set noerrorbells

"*****************************************************************************
" Indent Width
"*****************************************************************************
set expandtab "タブ入力を複数の空白入力に置き換える
set tabstop=4 "画面上でタブ文字が占める幅
set shiftwidth=4 "自動インデントでずれる幅
set softtabstop=4 "連続した空白に対してタブキーやバックスペースキーでカーソルが動く幅
set autoindent "改行時に前の行のインデントを継続する
set smartindent "改行時に入力された行の末尾に合わせて次の行のインデントを増減する
set smarttab "新しい行を作った時に高度な自動インデントを行う

autocmd Syntax * syn sync minlines=500 maxlines=1000
autocmd BufRead,BufNewFile *.go setfiletype go
autocmd FileType go setlocal noexpandtab
autocmd FileType dart setlocal ts=2 sw=2
autocmd FileType vue setlocal ts=2 sw=2
autocmd FileType typescript setlocal ts=2 sw=2
autocmd FileType typescriptreact setlocal ts=2 sw=2
autocmd FileType javascript setlocal ts=2 sw=2
autocmd FileType vim setlocal ts=2 sw=2
autocmd FileType lua setlocal ts=2 sw=2
autocmd FileType yaml setlocal ts=2 sw=2
autocmd FileType python setlocal ts=4 sw=4
autocmd FileType proto setlocal ts=2 sw=2
augroup vagrant
   au!
   au BufRead,BufNewFile Vagrantfile set filetype=ruby
 augroup END



"*****************************************************************************
" Other
"*****************************************************************************
" if hidden not set, TextEdit might fail.
set hidden
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=1


" don't give |ins-completion-menu| messages.
set shortmess=acTSFI

set signcolumn=yes

set incsearch                                    " サーチ：インクリメンタルサーチ（検索中に文字を打つと自動で検索していく）
set ignorecase                                   " サーチ：大文字小文字を区別しない
set smartcase                                    " サーチ：大文字で検索されたら対象を大文字限定にする
set showmatch                                    " カーソル：括弧にカーソルを合わせた時、対応した括弧を表示する
set noswapfile
" 内容が変更されたら自動で再読込
set autoread
set synmaxcol=200
" 正規表現エンジンの固定
" set regexpengine=1

if !&compatible
  set nocompatible
endif


" reset augroup
augroup vimrc
  autocmd!
augroup END

" lua require('plugins_packer')
lua require('plugins_lazy')

" load my scripts {{
lua << EOF
require('scripts/random_colorscheme').set_random_colorscheme({
  'everforest',
  'edge',
  'kanagawa',
  'onenord',
  'tokyonight',
  'gruvbox-flat',
  'nightfox',
  'duskfox',
  'nordfox',
  'kimbox',
  'everblush',
  })
require('scripts/gen_gitignore').initialize()
require('scripts/git_linker').initialize()
EOF

" my utils {{

" local vimrc
function! s:VimrcLocal(loc)
  let files = findfile('.vimrc.local', escape(a:loc, ' ') . ';', -1)
  for i in reverse(filter(files, 'filereadable(v:val)'))
    source `=i`
  endfor
endfunction
augroup VimrcLocal
  autocmd!
  autocmd BufNewFile,BufReadPost * call s:VimrcLocal(expand('<afile>:p:h'))
augroup END


" selected search when visual mode
function! VisualSearch()
  let reg = '"'
  let [save_reg, save_type] = [getreg(reg), getregtype(reg)]
  normal! gv""y
  let text = @"
  call setreg(reg, save_reg, save_type)

  let @/ = text
  call histadd('/', text)
endfunction
vnoremap <silent> * :<C-u>call VisualSearch()<CR>

let s:scratch_ft_map = {
      \ 'markdown': 'md',
      \ 'text': 'txt',
      \ 'python': 'py',
      \ 'json': 'json',
\ }

function! s:Scratch(ft, open) abort
  let l:ext = s:scratch_ft_map[a:ft]
  let l:bname = 'tkmpypy://scratch.' . l:ext
  execute a:open. ' ' . l:bname
  let &ft=a:ft
endfunction

function! s:ListScratchFiletypes(ArgLead, CmdLine, CursorPos) abort
  return s:scratch_ft_map->keys()
endfunction

command! -complete=customlist,s:ListScratchFiletypes -nargs=1 ScratchV :call s:Scratch(<f-args>, 'vsplit')
command! -complete=customlist,s:ListScratchFiletypes -nargs=1 ScratchS :call s:Scratch(<f-args>, 'split')
" }}

" }}
" vim-rooter {{
nnoremap <leader>cdr :Rooter<CR>
" }}

" iamcco/markdown-preview.nvim {{
" set to 1, nvim will open the preview window after entering the markdown buffer
" default: 0
let g:mkdp_auto_start = 0

" set to 1, the nvim will auto close current preview window when change
" from markdown buffer to another buffer
" default: 1
let g:mkdp_auto_close = 0

" set to 1, the vim will refresh markdown when save the buffer or
" leave from insert mode, default 0 is auto refresh markdown as you edit or
" move the cursor
" default: 0
let g:mkdp_refresh_slow = 0

" set to 1, the MarkdownPreview command can be use for all files,
" by default it can be use in markdown file
" default: 0
let g:mkdp_command_for_global = 1

" set to 1, preview server available to others in your network
" by default, the server listens on localhost (127.0.0.1)
" default: 0
let g:mkdp_open_to_the_world = 0

" use custom IP to open preview page
" useful when you work in remote vim and preview on local browser
" more detail see: https://github.com/iamcco/markdown-preview.nvim/pull/9
" default empty
let g:mkdp_open_ip = ''

" specify browser to open preview page
" default: ''
let g:mkdp_browser = ''

" set to 1, echo preview page url in command line when open preview page
" default is 0
let g:mkdp_echo_preview_url = 0

" a custom vim function name to open preview page
" this function will receive url as param
" default is empty
let g:mkdp_browserfunc = ''

" options for markdown render
" mkit: markdown-it options for render
" katex: katex options for math
" uml: markdown-it-plantuml options
" maid: mermaid options
" disable_sync_scroll: if disable sync scroll, default 0
" sync_scroll_type: 'middle', 'top' or 'relative', default value is 'middle'
"   middle: mean the cursor position alway show at the middle of the preview page
"   top: mean the vim top viewport alway show at the top of the preview page
"   relative: mean the cursor position alway show at the relative positon of the preview page
" hide_yaml_meta: if hide yaml metadata, default is 1
let g:mkdp_preview_options = {
    \ 'mkit': {},
    \ 'katex': {},
    \ 'uml': {},
    \ 'maid': {},
    \ 'disable_sync_scroll': 0,
    \ 'sync_scroll_type': 'middle',
    \ 'hide_yaml_meta': 1
    \ }

" use a custom markdown style must be absolute path
" let g:mkdp_markdown_css = 'https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css'

" use a custom highlight style must absolute path
let g:mkdp_highlight_css = ''

" use a custom port to start server or random for empty
let g:mkdp_port = ''

" preview page title
" ${name} will be replace with the file name
let g:mkdp_page_title = '「${name}」'

" nmap <leader>p <Plug>MarkdownPreview
" nmap <M-s> <Plug>MarkdownPreviewStop
" nmap <C-p> <Plug>MarkdownPreviewToggle
" }}
" vim-over {{
" over.vimの起動
nnoremap <silent> <Leader>rw :OverCommandLine<CR>

" カーソル下の単語をハイライト付きで置換
nnoremap <silent> <Leader>rc :OverCommandLine<CR>%s/<C-r><C-w>//g<Left><Left>

" コピーした文字列をハイライト付きで置換
nnoremap <silent> <Leader>ry y:OverCommandLine<CR>%s!<C-r>=substitute(@0, '!', '\\!', 'g')<CR>!!gI<Left><Left><Left>

" }}

"*****************************************************************************
" Utility Functions
"*****************************************************************************
function! s:register_path_relative()
  let @* = expand('%')
endfunction

function! s:register_path_absolute()
  let @* = expand('%:p')
endfunction

function! s:register_path_filename()
  let @* = expand('%:t')
endfunction

nnoremap <silent><leader>p  :call <SID>register_path_relative()<CR>
nnoremap <silent><leader>P  :call <SID>register_path_absolute()<CR>
nnoremap <silent><leader>pf  :call <SID>register_path_filename()<CR>

command! Profile call s:command_profile()
function! s:command_profile() abort
  profile start ~/profile.txt
  profile func *
  profile file *
endfunction


"*****************************************************************************
" KeyMap
"*****************************************************************************

" echo current buffer path
nnoremap <leader>ec :echo expand("%:p")<CR>

" move window
nnoremap <Leader>wmh <C-w>H
nnoremap <Leader>wmj <C-w>J
nnoremap <Leader>wmk <C-w>K
nnoremap <Leader>wml <C-w>L

" change buffer
nnoremap <Leader>bn :bnext<Enter>
nnoremap <Leader>bp :bprevious<Enter>
" nnoremap <Leader>bd :bdelete<Enter>
nnoremap <Leader>bra :bufdo e!<Enter>

" change tab
nnoremap <Leader>tn :tabn<Enter>
nnoremap <Leader>tp :tabp<Enter>

" noremap <S-h>   ^
" noremap <S-j>   }
" noremap <S-k>   {
" noremap <S-l>   $

" remap arrow keys
nnoremap j gj
nnoremap k gk


inoremap <C-c> <ESC><ESC>

" terminal
" ESCでターミナルモードからノーマルモードへ
tnoremap <C-W>N <C-\><C-N>

nnoremap <Leader>cdg :cd %:h<Enter>:pwd<Enter>
nnoremap <Leader>cdl :lcd %:h<Enter>:pwd<Enter>

