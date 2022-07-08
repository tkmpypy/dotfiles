scriptencoding=utf-8

let g:did_install_default_menus = 1
let g:did_install_syntax_menu   = 1
let g:did_indent_on             = 1
let g:did_load_ftplugin         = 1
let g:loaded_2html_plugin       = 1
let g:loaded_gzip               = 1
let g:loaded_man                = 1
let g:loaded_matchit            = 1
let g:loaded_matchparen         = 1
let g:loaded_netrwPlugin        = 1
let g:loaded_remote_plugins     = 1
let g:loaded_shada_plugin       = 1
let g:loaded_spellfile_plugin   = 1
let g:loaded_tarPlugin          = 1
let g:loaded_tutor_mode_plugin  = 1
let g:loaded_zipPlugin          = 1
let g:skip_loading_mswin        = 1
" enable filetype.lua and disable filetype.vim
let g:do_legacy_filetype=0

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

set ttyfast
set lazyredraw
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
set shortmess&
    \ shortmess+=c
    \ shortmess-=S

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


let mapleader = "\<Space>"

let g:use_treesitter = v:true
let g:lsp_client_type = 'neovim' " neovim(builtin), coc
let g:fuzzy_finder_type = 'telescope' " telescope, fzf
let g:git_client_type = 'neogit' " neogit, gina


" reset augroup
augroup vimrc
  autocmd!
augroup END

lua require('plugins')

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
  'doom-one'
  })
require('scripts/gen_gitignore').initialize()
require('scripts/git_linker').initialize()
EOF


nnoremap <silent> <leader>glc <cmd>GitLinker current<CR>
xnoremap <silent> <leader>glc <cmd>'<,'>GitLinker current<CR>
nnoremap <silent> <leader>gld <cmd>GitLinker default<CR>
xnoremap <silent> <leader>gld <cmd>'<,'>GitLinker default<CR>


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


" vim-quickrun {{
nnoremap <leader>rb :QuickRun<CR>
let g:quickrun_config = {}
let g:quickrun_config['typescript'] = { 'type' : 'typescript/tsc' }
let g:quickrun_config['typescript/tsc'] = {
\   'command': 'tsc',
\   'exec': ['%c --target esnext --module commonjs %o %s', 'node %s:r.js'],
\   'tempfile': '%{tempname()}.ts',
\   'hook/sweep/files': ['%S:p:r.js'],
\ }
let g:quickrun_config['rust'] = { 'type' : 'rust/cargo' }
let g:quickrun_config['rust/cargo'] = {
\   'command': 'cargo',
\   'exec': ['%c run'],
\ }
" }}
" vim-test {{
let g:test#echo_command = 1
let g:test#strategy = "neovim"
let g:test#neovim#term_position = "belowright"
let g:test#preserve_screen = 1
let g:test#javascript#runner = 'jest'
let g:test#javascript#jest#executable = './node_modules/.bin/jest'
let g:test#python#runner = 'pytest'
let g:test#python#pytest#options = {
    \ 'nearest': '-vv --capture=no',
    \ 'file': '-vv --capture=no'
\ }
let g:test#rust#cargotest#options = {
    \ 'nearest': '-- --nocapture'
\ }
let g:test#go#gotest#options = {
    \ 'nearest': '-v'
\ }
nmap <leader>trn :TestNearest<CR>
nmap <leader>trf :TestFile<CR>
nmap <leader>trs :TestSuite<CR>
nmap <leader>trr :TestLast<CR>
nmap <leader>trg :TestVisit<CR>
" }}

function! s:setup_vsnip()
  " NOTE: You can use other key to expand snippet.

  " vsip
  " Expand
  imap <expr> <C-y>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-y>'
  smap <expr> <C-y>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-y>'

  " Expand or jump
  imap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
  smap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'

  " Jump forward or backward
  imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
  smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
  imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
  smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

  " If you want to use snippet for multiple filetypes, you can `g:vsip_filetypes` for it.
  let g:vsnip_filetypes = {}
  let g:vsnip_filetypes.javascriptreact = ['javascript']
  let g:vsnip_filetypes.typescriptreact = ['typescript']
endfunction

function! LspStatus() abort
  if luaeval('#vim.lsp.buf_get_clients() > 0')
    return luaeval("require('lsp-status').status()")
  endif

  return ''
endfunction

function! s:setup_coc()
  " setting
    let g:coc_global_extensions = [
          \  'coc-lists'
          \, 'coc-diagnostic'
          \, 'coc-json'
          \, 'coc-yaml'
          \, 'coc-marketplace'
          \, 'coc-html'
          \, 'coc-css'
          \, 'coc-tsserver'
          \, 'coc-eslint'
          \, 'coc-prettier'
          \, 'coc-pyright'
          \, 'coc-rust-analyzer'
          \, 'coc-snippets'
          \, 'coc-vimlsp'
          \, 'coc-flutter-tools'
          \, 'coc-translator'
          \, 'coc-go'
          \, 'coc-lua'
          \, 'coc-sql'
          \, 'coc-sh'
          \, 'coc-emoji'
          \, 'coc-gitignore'
          \, 'coc-docker'
          \, 'coc-spell-checker'
          \, 'https://github.com/cstrap/python-snippets'
          \ ]
    function! CocCurrentFunction()
        let funcName = get(b:, 'coc_current_function', '')
        if funcName != ''
            let funcName = ' ' . funcName
        endif
        return funcName
    endfunction

    let g:coc_status_error_sign = "✘"
    let g:coc_status_warning_sign = "⚠"

    " " OR this mapping also breaks it in same manor
    " Make <cr> select the first completion item and confirm completion when no item have selected
    " " Use `[c` and `]c` to navigate diagnostics
    nmap <silent> <leader>dp <Plug>(coc-diagnostic-prev)
    nmap <silent> <leader>dn <Plug>(coc-diagnostic-next)

    " Remap keys for gotos
    nmap <silent> gd <Plug>(coc-definition)
    " nmap <silent> gy <Plug>(coc-type-definition)
    " nmap <silent> gi <Plug>(coc-implementation)
    " nmap <silent> gr <Plug>(coc-references)

    " Use K for show documentation in preview window
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    " Remap <C-f> and <C-b> for scroll float windows/popups.
    if has('nvim-0.4.0') || has('patch-8.2.0750')
      nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
      nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
      inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
      inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
      vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
      vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
    endif

    " Use <c-space> to trigger completion.
    if has('nvim')
      inoremap <silent><expr> <c-space> coc#refresh()
    else
      inoremap <silent><expr> <c-@> coc#refresh()
    endif

    function! s:show_documentation()
      if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
      elseif (coc#rpc#ready())
        call CocActionAsync('doHover')
      else
        execute '!' . &keywordprg . " " . expand('<cword>')
      endif
    endfunction

    " Highlight symbol under cursor on CursorHold
    autocmd CursorHold * silent call CocActionAsync('highlight')

    " Remap for rename current word
    nmap <leader>rn <Plug>(coc-rename)

    " Remap for format selected region
    " xmap <leader>f  <Plug>(coc-format-selected)
    " nmap <leader>f  <Plug>(coc-format-selected)

    augroup mygroup
      autocmd!
      " Setup formatexpr specified filetype(s).
      autocmd FileType typescript,json setl formatexpr=CocActionAsync('formatSelected')
      " Update signature help on jump placeholder
      autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    augroup end

    " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
    xmap <leader>as  <Plug>(coc-codeaction-selected)<CR>
    nmap <leader>as  <Plug>(coc-codeaction-selected)<CR>

    " Remap for do codeAction of current line
    nmap <leader>ac  <Plug>(coc-codeaction)
    nmap <leader>aC  <Plug>(coc-codelens-action)
    " " Fix autofix problem of current line
    nmap <leader>qf  <Plug>(coc-fix-current)

    " Introduce function text object
    " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
    xmap if <Plug>(coc-funcobj-i)
    xmap af <Plug>(coc-funcobj-a)
    omap if <Plug>(coc-funcobj-i)
    omap af <Plug>(coc-funcobj-a)

    " Use <TAB> for selections ranges.
    " NOTE: Requires 'textDocument/selectionRange' support from the language server.
    " coc-tsserver, coc-python are the examples of servers that support it.
    nmap <silent> <TAB> <Plug>(coc-range-select)
    xmap <silent> <TAB> <Plug>(coc-range-select)


    " Use `:Fold` to fold current buffer
    command! -nargs=? Fold :call     CocActionAsync('fold', <f-args>)
    " " use `:OR` for organize import of current buffer
    command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

    " Using CocList
    " Show all diagnostics
    " nnoremap <silent> <leader>d  :<C-u>CocList diagnostics<cr>
    " " Manage extensions
    " nnoremap <silent> <leader>e  :<C-u>CocList extensions<cr>
    " Show commands
    " nnoremap <silent> <leader>c  :<C-u>CocList commands<cr>
    " Find symbol of current document
    " nnoremap <silent> <leader>o  :<C-u>CocList outline<cr>
    " Search workspace symbols
    " nnoremap <silent> <leader>S  :<C-u>CocList -I symbols<cr>
    " Do default action for next item.
    " nnoremap <silent> <leader>j  :<C-u>CocNext<CR>
    " Do default action for previous item.
    " nnoremap <silent> <leader>k  :<C-u>CocPrev<CR>
    " Resume latest coc list
    " nnoremap <silent> <leader>p  :<C-u>CocListResume<CR>
    " coc-yank
    " nnoremap <silent> <leader>y  :<C-u>CocList -A --normal yank<cr>

    " Use `:Format` to format current buffer
    " command! -nargs=0 Format :call CocActionAsync('format')
    " nnoremap <silent> <leader>F  :<C-u>Format<cr>

    nnoremap <silent> <leader>I  :<C-u>OR<cr>
    " coc-translator
    " popup
    nmap <Leader>tr <Plug>(coc-translator-p)
    vmap <Leader>tr <Plug>(coc-translator-pv)

    " Make <CR> auto-select the first completion item and notify coc.nvim to
    " format on enter, <cr> could be remapped by other vim plugin
    inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                                  \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

    " Use <C-l> for trigger snippet expand.
    imap <C-l> <Plug>(coc-snippets-expand)

    " Use <C-j> for select text for visual placeholder of snippet.
    vmap <C-j> <Plug>(coc-snippets-select)

    " Use <C-j> for jump to next placeholder, it's default of coc.nvim
    let g:coc_snippet_next = '<c-j>'

    " Use <C-k> for jump to previous placeholder, it's default of coc.nvim
    let g:coc_snippet_prev = '<c-k>'

    " Use <C-j> for both expand and jump (make expand higher priority.)
    imap <C-j> <Plug>(coc-snippets-expand-jump)

    " Use <leader>x for convert visual selected code to snippet
    xmap <leader>x  <Plug>(coc-convert-snippet)

endfunction


if g:lsp_client_type == 'neovim'
  call s:setup_vsnip()
elseif g:lsp_client_type == 'coc'
  call s:setup_coc()
endif

" }}
" vim-rooter {{
nnoremap <leader>cdr :Rooter<CR>
" }}

" liuchengxu/vista.vim {{
" How each level is indented and what to prepend.
" This could make the display more compact or more spacious.
" e.g., more compact: ["▸ ", ""]
let g:vista_icon_indent = ["╰─▸ ", "├─▸ "]

" Executive used when opening vista sidebar without specifying it.
" See all the avaliable executives via `:echo g:vista#executives`.


if g:lsp_client_type == 'neovim'
  let g:vista_default_executive = 'nvim_lsp'
elseif g:lsp_client_type == 'coc'
  let g:vista_default_executive = 'coc'
endif
" Set the executive for some filetypes explicitly. Use the explicit executive
" instead of the default one for these filetypes when using `:Vista` without
" specifying the executive.
" let g:vista_executive_for = {
"   \ 'cpp': 'vim_lsp',
"   \ 'php': 'vim_lsp',
"   \ }

" Declare the command including the executable and options used to generate ctags output
" for some certain filetypes.The file path will be appened to your custom command.
" For example:
" let g:vista_ctags_cmd = {
"       \ 'haskell': 'hasktags -x -o - -c',
"       \ }
" To enable fzf's preview window set g:vista_fzf_preview.
" The elements of g:vista_fzf_preview will be passed as arguments to fzf#vim#with_preview()
" For example:
let g:vista_fzf_preview = ['right:50%']
" Ensure you have installed some decent font to show these pretty symbols, then you can enable icon for the kind.
let g:vista#renderer#enable_icon = 1
nnoremap <leader>tc :Vista coc<CR>
nnoremap <leader>tt :Vista!! <CR>
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
" pechorin/any-jump.vim{{
nnoremap <Leader>j :AnyJump<CR>
" }}

" chowcho.nvim {{
nnoremap <leader>ww :Chowcho<CR>

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

" resize wondiw mode
nnoremap <Leader>wr :WinResizerStartResize<Enter>

" noremap <S-h>   ^
" noremap <S-j>   }
" noremap <S-k>   {
" noremap <S-l>   $

" remap arrow keys
nnoremap j gj
nnoremap k gk


inoremap <C-c> <ESC>

" terminal
" ESCでターミナルモードからノーマルモードへ
tnoremap <C-W>N <C-\><C-N>

nnoremap <Leader>cdg :cd %:h<Enter>:pwd<Enter>
nnoremap <Leader>cdl :lcd %:h<Enter>:pwd<Enter>

