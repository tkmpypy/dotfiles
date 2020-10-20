let g:polyglot_disabled = ['markdown', 'python', 'lua', 'go', 'ruby', 'rust', 'html', 'toml', 'json', 'yaml']
" let g:polyglot_disabled = ['markdown','md', 'lua']

" Disable unnecessary default plugins
" let g:loaded_gzip              = 1
" let g:loaded_tar               = 1
" let g:loaded_tarPlugin         = 1
" let g:loaded_zip               = 1
" let g:loaded_zipPlugin         = 1
let g:loaded_rrhelper          = 1
" let g:loaded_2html_plugin      = 1
let g:loaded_vimball           = 1
let g:loaded_vimballPlugin     = 1
let g:loaded_getscript         = 1
let g:loaded_getscriptPlugin   = 1
let g:loaded_logipat           = 1
let g:loaded_matchparen        = 1
let g:loaded_man               = 1
" NOTE:
" The Netrw is use to download a missing spellfile
let g:loaded_netrw             = 1
let g:loaded_netrwPlugin       = 1
let g:loaded_netrwSettings     = 1
let g:loaded_netrwFileHandlers = 1

filetype plugin indent on
syntax on

if !&compatible
  set nocompatible
endif

" reset augroup
augroup MyAutoCmd
  autocmd!
augroup END

" vim-plugの自動インストール
let s:plug_dir = '~/.vim/plugged'
if has('nvim')
  if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif
else
  if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
     \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif
endif

if executable('tmux')  && $TMUX !=# ''
    let g:vimIsInTmux = 1
else
    let g:vimIsInTmux = 0
endif

" Install plugins
call plug#begin(s:plug_dir)
" Colors
Plug 'altercation/vim-colors-solarized'
Plug 'Rigellute/rigel'
Plug 'w0ng/vim-hybrid'
Plug 'flrnprz/plastic.vim'
Plug 'KeitaNakamura/neodark.vim'
Plug 'joshdick/onedark.vim'
Plug 'sonph/onehalf'
Plug 'cocopon/iceberg.vim'
Plug 'dracula/vim'
Plug 'morhetz/gruvbox'
Plug 'kaicataldo/material.vim'
Plug 'edkolev/tmuxline.vim'
Plug 'sainnhe/edge'
Plug 'sainnhe/gruvbox-material'
Plug 'arcticicestudio/nord-vim'
Plug 'whatyouhide/vim-gotham'
Plug 'yuttie/hydrangea-vim'
Plug 'NLKNguyen/papercolor-theme'
Plug 'junegunn/seoul256.vim'
Plug 'ghifarit53/tokyonight-vim'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'franbach/miramare'

Plug 'luochen1990/rainbow'
" <leader>qでアクティブなBufferをキル（windowはそのまま）
Plug 'moll/vim-bbye'
" gccでコメントアウト
Plug 'tyru/caw.vim'
Plug 'thinca/vim-quickrun'
Plug 'godlygeek/tabular'
" Languages
Plug 'plasticboy/vim-markdown', {'for': 'markdown'}
Plug 'thosakwe/vim-flutter'
Plug 'sheerun/vim-polyglot'
Plug 'metakirby5/codi.vim'
Plug 'vim-test/vim-test'

Plug 'wakatime/vim-wakatime'

" Completion
if has('nvim')
    
    " perform
    Plug 'antoinemadec/FixCursorHold.nvim'
    Plug 'nvim-treesitter/nvim-treesitter'

    " use coc.nvim
    " Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}

    " use neovim built-in
    Plug 'neovim/nvim-lspconfig'
    Plug 'nvim-lua/completion-nvim'
    Plug 'steelsojka/completion-buffers'
    Plug 'nvim-lua/diagnostic-nvim'
    Plug 'nvim-lua/lsp-status.nvim'
    Plug 'hrsh7th/vim-vsnip'
    Plug 'hrsh7th/vim-vsnip-integ'
    
    Plug 'RishabhRD/popfix'
    Plug 'RishabhRD/nvim-lsputils'
    Plug 'tjdevries/lsp_extensions.nvim'

    " use vim-lsp
    " Plug 'prabirshrestha/vim-lsp'
    " Plug 'mattn/vim-lsp-settings'
    " Plug 'prabirshrestha/asyncomplete-lsp.vim'
    " use asyncomplete
    " Plug 'prabirshrestha/asyncomplete-buffer.vim'
    " Plug 'prabirshrestha/asyncomplete-file.vim'
    " Plug 'prabirshrestha/asyncomplete.vim'

    " use ale
    " Plug 'dense-analysis/ale'
    " Plug 'maximbaz/lightline-ale'

    " explorer
    Plug 'kyazdani42/nvim-web-devicons' " for file icons
    Plug 'kyazdani42/nvim-tree.lua'

    " lang
    Plug 'tjdevries/nlua.nvim'
    Plug 'rafcamlet/nvim-luapad'

    " finder
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-lua/telescope.nvim'
else
    " use coc.nvim
    Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
    " use vim-lsp
    " Plug 'prabirshrestha/vim-lsp'
    " Plug 'mattn/vim-lsp-settings'
    " Plug 'dense-analysis/ale'
    " Plug 'maximbaz/lightline-ale'
    " use asyncomplete
    " Plug 'prabirshrestha/asyncomplete.vim'
    " Plug 'prabirshrestha/asyncomplete-lsp.vim'
    " Plug 'prabirshrestha/asyncomplete-buffer.vim'
    " Plug 'prabirshrestha/asyncomplete-file.vim'

    " explorer
    Plug 'lambdalisue/fern.vim'
    Plug 'lambdalisue/fern-renderer-nerdfont.vim'
    Plug 'lambdalisue/fern-git-status.vim'
    Plug 'lambdalisue/fern-mapping-git.vim'

    " finder
    Plug 'junegunn/fzf', { 'do': './install --all' }
      Plug 'junegunn/fzf.vim'

endif

" lang
Plug 'euclidianAce/BetterLua.vim'

" Visual
Plug 'yggdroot/indentline'
Plug 'itchyny/lightline.vim'
Plug 'mengelbrecht/lightline-bufferline'
Plug 'lambdalisue/nerdfont.vim'
Plug 'lambdalisue/glyph-palette.vim'
" Plug 'hardcoreplayers/dashboard-nvim'
Plug 'mhinz/vim-startify'
Plug 'liuchengxu/vista.vim'

Plug 'airblade/vim-rooter'

" Util
Plug 'MattesGroeger/vim-bookmarks'
Plug 'machakann/vim-sandwich'
Plug 'simeji/winresizer'
Plug 'cohama/lexima.vim'
Plug 'mattn/webapi-vim'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
Plug 'glidenote/memolist.vim'
Plug 'mbbill/undotree'
Plug 'osyo-manga/vim-over'
Plug 'rhysd/devdocs.vim'
Plug 'tyru/operator-camelize.vim'
  Plug 'kana/vim-operator-user'
Plug 't9md/vim-choosewin'
Plug 'pechorin/any-jump.vim'
Plug 'hrsh7th/vim-eft'

" Git
Plug 'lambdalisue/gina.vim'
Plug 'mhinz/vim-signify'
Plug 'gotchane/vim-git-commit-prefix'
Plug 'APZelos/blamer.nvim'

call plug#end()

let s:plug = get(g:, 'plugs', {})

function! s:plug.is_installed(name)
  return has_key(s:plug, a:name) ? isdirectory(s:plug[a:name].dir) : 0
endfunction

let mapleader = "\<Space>"

if s:plug.is_installed('FixCursorHold.nvim')
  let g:cursorhold_updatetime = 100
endif

" telescope.nvim {{
function! s:init_telescope()
  " bat (preview) * 
  " ripgrep (finder) * 
  " Treesitter (nvim-treesitter) (finder/preview)
  " fd (sharkdp/fd) (finder)
  " git (picker) * 
  " neovim LSP (picker)
  " devicons 
  lua require('telescope_settings')
  nnoremap <Leader>sf <cmd>lua require'telescope.builtin'.git_files{}<CR>
  nnoremap <Leader>sF <cmd>lua require'telescope.builtin'.find_files{ find_command = {"rg", "-i", "--hidden", "--files", "-g", "!.git"} }<CR>
  nnoremap <Leader>sgr <cmd>lua require'telescope.builtin'.lsp_references{}<CR>
  nnoremap <Leader>ss <cmd>lua require'telescope.builtin'.lsp_workspace_symbols{}<CR>
  nnoremap <Leader>sg <cmd>lua require'telescope.builtin'.live_grep{}<CR>
  nnoremap <Leader>sb <cmd>lua require'telescope.builtin'.buffers{}<CR>
  nnoremap <Leader>sc <cmd>lua require'telescope.builtin'.command_history{}<CR>
  nnoremap <Leader>sr <cmd>lua require'telescope.builtin'.oldfiles{}<CR>
  nnoremap <Leader>sl <cmd>lua require'telescope.builtin'.loclist{}<CR>
  nnoremap <Leader>sq <cmd>lua require'telescope.builtin'.quickfix{}<CR>
endfunction

if s:plug.is_installed('telescope.nvim')
  call s:init_telescope()
endif
" }}


" fzf {{
function! s:init_fzf()
  " Default fzf layout
  " - down / up / left / right
  " let g:fzf_layout = { 'down': '~40%' }
  let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6 } }

  " [Buffers] Jump to the existing window if possible
  let g:fzf_buffers_jump = 1

  " [[B]Commits] Customize the options used by 'git log':
  let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'

  " [Tags] Command to generate tags file
  let g:fzf_tags_command = 'ctags -R'

  " [Commands] --expect expression for directly executing the command
  let g:fzf_commands_expect = 'alt-enter,ctrl-x'
  " Command for git grep
  " - fzf#vim#grep(command, with_column, [options], [fullscreen])
  command! -bang -nargs=* GGrep
    \ call fzf#vim#grep(
    \   'git grep --line-number '.shellescape(<q-args>), 0,
    \   fzf#vim#with_preview({'options': '--exact --reverse --delimiter : --nth 3..'}), <bang>0)
    " \   { 'dir': systemlist('git rev-parse --show-toplevel')[0] }, <bang>0)

  " Override Colors command. You can safely do this in your .vimrc as fzf.vim
  " will not override existing commands.
  command! -bang Colors
    \ call fzf#vim#colors({'left': '15%', 'options': '--reverse --margin 30%,0'}, <bang>0)

  " Augmenting Ag command using fzf#vim#with_preview function
  "   * fzf#vim#with_preview([[options], [preview window], [toggle keys...]])
  "     * For syntax-highlighting, Ruby and any of the following tools are required:
  "       - Bat: https://github.com/sharkdp/bat
  "       - Highlight: http://www.andre-simon.de/doku/highlight/en/highlight.php
  "       - CodeRay: http://coderay.rubychan.de/
  "       - Rouge: https://github.com/jneen/rouge
  "
  "   :Ag  - Start fzf with hidden preview window that can be enabled with "?" key
  "   :Ag! - Start fzf in fullscreen and display the preview window above
  command! -bang -nargs=* Ag
    \ call fzf#vim#ag(<q-args>,
    \                 <bang>0 ? fzf#vim#with_preview({'options': '--exact --reverse --delimiter : --nth 3..'},'up:60%')
    \                         : fzf#vim#with_preview({'options': '--exact --reverse --delimiter : --nth 3..'},'right:50%:hidden', '?'),
    \                 <bang>0)

  " Similarly, we can apply it to fzf#vim#grep. To use ripgrep instead of ag:
  command! -bang -nargs=* Rg
    \ call fzf#vim#grep(
    \   'rg -S --column --hidden --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
    \   <bang>0 ? fzf#vim#with_preview({'options': '--exact --reverse --delimiter : --nth 3..'},'up:60%')
    \           : fzf#vim#with_preview({'options': '--exact --reverse --delimiter : --nth 3..'},'right:50%:hidden', '?'),
    \   <bang>0)

  " Likewise, Files command with preview window
  command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
  command! -bang -nargs=? -complete=dir GFiles
    \ call fzf#vim#gitfiles(<q-args>, fzf#vim#with_preview(), <bang>0)

  nnoremap <leader>sb :<C-u>Buffers<CR>
  nnoremap <leader>sx :<C-u>Commands<CR>
  nnoremap <leader>sf :<C-u>GFiles<CR>
  nnoremap <leader>sF :<C-u>Files<CR>
  nnoremap <leader>sc :<C-u>Commits<CR>
  nnoremap <leader>sm :<C-u>Marks<CR>
  nnoremap <leader>scb :<C-u>BCommits<CR>
  nnoremap <leader>sg :<C-u>Rg<CR>
  nnoremap <leader>sG :<C-u>GGrep<CR>
  nnoremap <leader>sr :History<CR>
  nnoremap <leader>sgs :<C-u>GFiles?<CR>
endfunction

if s:plug.is_installed('fzf.vim')
  call s:init_fzf()
endif


" }}
" vim-bbye {{
nnoremap <leader>q :Bdelete<CR>
nnoremap <leader>qq :Bdelete!<CR>
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

if has('nvim')
    let test#strategy = "neovim"
else
    let test#strategy = "vimterminal"
endif
let g:test#python#runner = 'pytest'
let g:test#rust#cargotest#options = {
    \ 'nearest': '-- --nocapture'
\ }
nmap <leader>trn :TestNearest<CR>
nmap <leader>trf :TestFile<CR>
nmap <leader>trs :TestSuite<CR>
nmap <leader>trr :TestLast<CR>
nmap <leader>trg :TestVisit<CR>
" }}
" vim-markdown {{
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_conceal = 0
let g:vim_markdown_math = 1
let g:vim_markdown_conceal_code_blocks = 0
nmap <leader>mit :InsertToc<CR>
nmap <leader>mt :Toc<CR>
nmap <leader>mf :TableFormat<CR>
" }}
" thosakwe/vim-flutter {{
nnoremap <leader>fa :FlutterRun<cr>
nnoremap <leader>fq :FlutterQuit<cr>
nnoremap <leader>fr :FlutterHotReload<cr>
nnoremap <leader>fR :FlutterHotRestart<cr>
" }}
" rainbow {{
let g:rainbow_active = 1
" }}
" python-syntax {{
let g:python_highlight_all = 1
" }}

function! s:set_nvim_lsp_diagnostic_color()
    hi! LspDiagnosticsError guifg=#FF0000 guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsWarning guifg=#FFDD00 guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsInformation guifg=#02DB1F guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsHint guifg=#02DAF2 guibg=NONE guisp=NONE gui=NONE cterm=bold
endfunction


function! s:setup_nvim_lsp()
    lua require('lsp_settings')
    let g:diagnostic_enable_virtual_text = 1
    let g:diagnostic_enable_underline = 1
    " let g:diagnostic_trimmed_virtual_text = '20'
    let g:space_before_virtual_text = 5
    let g:diagnostic_auto_popup_while_jump = 1
    let g:diagnostic_insert_delay = 1
    let g:diagnostic_show_sign = 1

    call sign_define("LspDiagnosticsErrorSign", {"text" : "✘", "texthl" : "LspDiagnosticsError"})
    call sign_define("LspDiagnosticsWarningSign", {"text" : "⚠", "texthl" : "LspDiagnosticsWarning"})
    call sign_define("LspDiagnosticsInformationSign", {"text" : "כֿ", "texthl" : "LspDiagnosticsInformation"})
    call sign_define("LspDiagnosticsHintSign", {"text" : "•", "texthl" : "LspDiagnosticsHint"})
    nnoremap <silent> gD    <cmd>lua vim.lsp.buf.declaration()<CR>
    nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
    nnoremap <silent> pd    <cmd>lua vim.lsp.buf.peek_definition()<CR>
    nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
    nnoremap <silent> gi    <cmd>lua vim.lsp.buf.implementation()<CR>
    nnoremap <silent> H     <cmd>lua vim.lsp.buf.signature_help()<CR>
    nnoremap <silent> gy   <cmd>lua vim.lsp.buf.type_definition()<CR>
    nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
    nnoremap <leader>rn    <cmd>lua vim.lsp.buf.rename()<CR>
    nnoremap <leader>ac    <cmd>lua vim.lsp.buf.code_action()<CR>
    nnoremap <leader>F    <cmd>lua vim.lsp.buf.formatting()<CR>

    autocmd ColorScheme * call s:set_nvim_lsp_diagnostic_color()
    autocmd InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost *.rs :lua require'lsp_extensions'.inlay_hints{ prefix = ' » ', highlight = "NonText" }

endfunction

function! s:setup_complete_nvim()
    " let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy', 'all']
    let g:completion_matching_strategy_list = ['exact']
    let g:completion_enable_snippet         = 'vim-vsnip'
    let g:completion_sorting = "length"
    let g:completion_matching_ignore_case = 1
    let g:completion_enable_auto_hover = 1
    let g:completion_enable_auto_signature = 1
    let g:completion_max_items = 20
    let g:completion_trigger_character = ['.', '::']
    let g:completion_chain_complete_list = {
                \ 'default': {
                \   'default': [
                \      {'complete_items': ['lsp', 'snippet']},
                \      {'complete_items': ['buffer', 'buffers']},
                \      {'complete_items': ['path']},
                \      {'mode': '<c-p>'},
                \      {'mode': '<c-n>'},
                \   ],
                \ },
                \ }
    let g:completion_auto_change_source = 1
    let g:completion_customize_lsp_label = {
          \  'Function'      : "",
          \  'Method'        : "",
          \  'Variable'      : "",
          \  'Constant'      : "",
          \  'Struct'        : "פּ",
          \  'Class'         : "",
          \  'Interface'     : "禍",
          \  'Text'          : "",
          \  'Enum'          : "",
          \  'EnumMember'    : "",
          \  'Module'        : "",
          \  'Color'         : "",
          \  'Property'      : "襁",
          \  'Field'         : "綠",
          \  'Unit'          : "",
          \  'File'          : "",
          \  'Value'         : "",
          \  'Event'         : "鬒",
          \  'Folder'        : "",
          \  'Keyword'       : "",
          \  'Snippet'       : "",
          \  'Operator'      : "洛",
          \  'Reference'     : " ",
          \  'TypeParameter' : "",
          \  'Default'       : ""
    \ }
    imap <c-j> <Plug>(completion_next_source)
    imap <c-k> <Plug>(completion_prev_source)

endfunction

function! LspStatus() abort
  if luaeval('#vim.lsp.buf_get_clients() > 0')
    return luaeval("require('lsp-status').status()")
  endif

  return ''
endfunction

function! s:setup_vim_lsp()
    let g:lsp_settings_filetype_python = 'pyls-ms'
    let g:lsp_diagnostics_enabled = 1
    let g:lsp_signs_enabled = 1         " enable signs
    let g:lsp_diagnostics_echo_cursor = 1 " enable echo under cursor when in normal mode
    let g:lsp_highlights_enabled = 1
    let g:lsp_textprop_enabled = 1
    let g:lsp_highlight_references_enabled = 1
    let g:lsp_virtual_text_enabled = 0
    highlight lspReference ctermfg=red guifg=red ctermbg=green guibg=green
    let g:lsp_settings_filetype_typescript = ['typescript-language-server', 'eslint-language-server']
    let g:lsp_settings_filetype_typescriptreact = ['typescript-language-server', 'eslint-language-server']

    let g:lsp_signs_error = {'text': '✘'}
    let g:lsp_signs_warning = {'text': '⚠'} " icons require GUI
    let g:lsp_signs_hint = {'test': '•'} " icons require GUI
    command! -nargs=0 OR call execute('LspCodeActionSync source.organizeImports')
    nmap <leader>rn :LspRename<cr>
    nmap <silent> gd :tab LspDefinition<cr>
    nmap <silent> pd :LspPeekDefinition<cr>
    nmap <silent> gy :tab LspTypeDefinition<cr>
    nmap <silent> gi :tab LspImplementation<cr>
    nmap <silent> gr :LspReferences<cr>
    nmap <silent> gh :LspSignatureHelp<cr>
    nnoremap <silent> K :LspHover<CR>
    nmap <leader>qf  :LspCodeAction<cr>
    nmap <leader>F  :LspDocumentFormat<cr>
    nmap <leader>I  :OR<cr>
endfunction

function! s:setup_coc()
  " setting
    let g:coc_global_extensions = [
          \  'coc-lists'
          \, 'coc-json'
          \, 'coc-yaml'
          \, 'coc-marketplace'
          \, 'coc-html'
          \, 'coc-css'
          \, 'coc-tsserver'
          \, 'coc-eslint'
          \, 'coc-prettier'
          \, 'coc-markdownlint'
          \, 'coc-python'
          \, 'coc-rust-analyzer'
          \, 'coc-snippets'
          \, 'coc-vimlsp'
          \, 'coc-flutter'
          \, 'coc-translator'
          \, 'coc-go'
          \, 'coc-lua'
          \, 'coc-sql'
          \, 'coc-emoji'
          \, 'coc-gitignore'
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
    nmap <silent> [c <Plug>(coc-diagnostic-prev)
    nmap <silent> ]c <Plug>(coc-diagnostic-next)

    " Remap keys for gotos
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)

    " Use K for show documentation in preview window
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    function! s:show_documentation()
      if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
      else
        call CocActionAsync('doHover')
      endif
    endfunction

    " Highlight symbol under cursor on CursorHold
    autocmd CursorHold * silent call CocActionAsync('highlight')

    " Remap for rename current word
    nmap <leader>rn <Plug>(coc-rename)

    " Remap for format selected region
    xmap <leader>f  <Plug>(coc-format-selected)
    nmap <leader>f  <Plug>(coc-format-selected)

    augroup mygroup
      autocmd!
      " Setup formatexpr specified filetype(s).
      autocmd FileType typescript,json setl formatexpr=CocActionAsync('formatSelected')
      " Update signature help on jump placeholder
      autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    augroup end

    " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
    xmap <space>a  <Plug>(coc-codeaction-selected)
    nmap <space>a  <Plug>(coc-codeaction-selected)

    " Remap for do codeAction of current line
    nmap <space>ac  <Plug>(coc-codeaction)
    " " Fix autofix problem of current line
    nmap <space>qf  <Plug>(coc-fix-current)

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

    " Use `:Format` to format current buffer
    command! -nargs=0 Format :call CocActionAsync('format')

    " Use `:Fold` to fold current buffer
    command! -nargs=? Fold :call     CocActionAsync('fold', <f-args>)
    " " use `:OR` for organize import of current buffer
    command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

    " Using CocList
    " Show all diagnostics
    nnoremap <silent> <space>d  :<C-u>CocList diagnostics<cr>
    " " Manage extensions
    nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
    " Show commands
    nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
    " Find symbol of current document
    nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
    " Search workspace symbols
    nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
    " Do default action for next item.
    nnoremap <silent> <space>j  :<C-u>CocNext<CR>
    " Do default action for previous item.
    nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
    " Resume latest coc list
    nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
    " coc-yank
    nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>
    nnoremap <silent> <space>F  :<C-u>Format<cr>
    nnoremap <silent> <space>I  :<C-u>OR<cr>
    " coc-translator
    " popup
    nmap <Leader>tr <Plug>(coc-translator-p)
    vmap <Leader>tr <Plug>(coc-translator-pv)

endfunction

function! s:setup_asyncomplete()
    let g:asyncomplete_auto_popup = 1
    let g:asyncomplete_popup_delay = 0
    let g:asyncomplete_smart_completion = 1
    let g:asyncomplete_remove_duplicates = 1
    " buffer
    call asyncomplete#register_source(asyncomplete#sources#buffer#get_source_options({
        \ 'name': 'buffer',
        \ 'whitelist': ['*'],
        \ 'priority': 1000,
        \ 'completor': function('asyncomplete#sources#buffer#completor'),
        \ 'config': {
        \    'max_buffer_size': 500,
        \  },
        \ }))
    " file
    au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
        \ 'name': 'file',
        \ 'whitelist': ['*'],
        \ 'priority': 1001,
        \ 'config': {
        \    'max_buffer_size': 500,
        \  },
        \ 'completor': function('asyncomplete#sources#file#completor')
        \ }))
    if s:plug.is_installed('asyncomplete_neovim_lsp')
        au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#lsp#get_source_options({}))
    endif
endfunction

if s:plug.is_installed('nvim-lspconfig')
    call s:setup_nvim_lsp()
    if s:plug.is_installed('asyncomplete.vim')
        call s:setup_asyncomplete()
    elseif s:plug.is_installed('completion-nvim')
        call s:setup_complete_nvim()
    endif

else
    if s:plug.is_installed('coc.nvim')
        call s:setup_coc()
    endif

    if s:plug.is_installed('vim-lsp')
        call s:setup_vim_lsp()
        if s:plug.is_installed('asyncomplete.vim')
            call s:setup_asyncomplete()
        endif

        if executable('efm-langserver') && !s:plug.is_installed('ale')
            augroup LspEFM
              au!
              autocmd User lsp_setup call lsp#register_server({
                  \ 'name': 'efm-langserver',
                  \ 'cmd': {server_info->['efm-langserver', '-c=' . $HOME . '/.config/efm-langserver/config.yaml']},
                  \ 'whitelist': ['vim', 'eruby', 'markdown', 'yaml', 'python', 'go', 'rust'],
                  \ })
            augroup END
        endif
    endif
endif

if s:plug.is_installed('ale')
    " ale {{
    let g:ale_fixers = {
    \   '*': ['remove_trailing_lines', 'trim_whitespace'],
    \   'python': ['autopep8'],
    \   'typescript': ['prettier', 'eslint'],
    \   'typescriptreact': ['prettier', 'eslint'],
    \   'javascript': ['prettier', 'eslint'],
    \   'javascriptreact': ['prettier', 'eslint'],
    \   'rust': ['rustfmt'],
    \   'go': ['gofmt', 'goimports'],
    \   'markdown': ['textlint'],
    \}
    let g:ale_linters = {
    \   '*': ['remove_trailing_lines', 'trim_whitespace'],
    \   'python': ['flake8'],
    \   'typescript': ['eslint'],
    \   'typescriptreact': ['eslint'],
    \   'javascript': ['eslint'],
    \   'javascriptreact': ['eslint'],
    \   'rust': [''],
    \   'go': ['golint', 'govet', 'gofmt'],
    \   'vim': ['vint'],
    \   'markdown': ['textlint'],
    \   'lua': ['luac', 'luacheck'],
    \}
    if executable('eslint_d')
      let g:ale_javascript_eslint_use_global = 1
      let g:ale_javascript_eslint_executable = 'eslint_d'
      let g:ale_javascriptreact_eslint_use_global = 1
      let g:ale_javascriptreact_eslint_executable = 'eslint_d'
      let g:ale_typescript_eslint_use_global = 1
      let g:ale_typescript_eslint_executable = 'eslint_d'
      let g:ale_typescriptreact_eslint_use_global = 1
      let g:ale_typescriptreact_eslint_executable = 'eslint_d'
    else
      let g:ale_javascript_eslint_use_global = 1
      let g:ale_javascript_eslint_executable = 'yarn'
      let g:ale_javascript_eslint_option = 'run eslint'
      let g:ale_javascriptreact_eslint_use_global = 1
      let g:ale_javascriptreact_eslint_executable = 'yarn'
      let g:ale_javascriptreact_eslint_option = 'run eslint'
      let g:ale_typescript_eslint_use_global = 1
      let g:ale_typescript_eslint_executable = 'yarn'
      let g:ale_typescript_eslint_option = 'run eslint'
      let g:ale_typescriptreact_eslint_use_global = 1
      let g:ale_typescriptreact_eslint_executable = 'yarn'
      let g:ale_typescriptreact_eslint_option = 'run eslint'
    endif
    let g:ale_linters_explicit = 1
    let g:ale_sign_error = '✘'
    let g:ale_sign_warning = '⚠'
    let g:ale_set_highlights = 1
    let g:ale_echo_msg_error_str = 'E'
    let g:ale_echo_msg_warning_str = 'W'
    let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
    " Write this in your vimrc file
    let g:ale_lint_on_text_changed = 1
    let g:ale_lint_on_insert_leave = 1
    " You can disable this option too
    " if you don't want linters to run on opening a file
    let g:ale_lint_on_enter = 1
    let g:ale_fix_on_save = 0
    nmap <leader>F  :ALEFix<cr>
    " }}
endif
" vim-json {{
let g:vim_json_syntax_conceal = 0
" }}
" indentLine {{
let g:indentLine_concealcursor = 'inc'
let g:indentLine_conceallevel = 2
" }}

let g:git_icon = ' '
function! GetBranchName()
    return g:git_icon . gina#component#repo#branch()
endfunction
" blamer.nvim{{
let g:blamer_enabled = 0
let g:blamer_delay = 1000
let g:blamer_show_in_visual_modes = 1
let g:blamer_prefix = '  '
" Available options: <author>, <author-mail>, <author-time>, <committer>, <committer-mail>, <committer-time>, <summary>, <commit-short>, <commit-long>.
let g:blamer_template = '<committer>, <committer-time> • <summary>'
nnoremap <Leader>gbt :BlamerToggle<CR>
" }}
" taohexxx/lightline-buffer {{
let g:lightline#bufferline#enable_nerdfont = 1
let g:lightline#bufferline#show_number  = 2
let g:lightline#bufferline#shorten_path = 0
let g:lightline#bufferline#unnamed      = '[No Name]'
let g:lightline#bufferline#filename_modifier = ':t'
let g:lightline#bufferline#unicode_symbols = 1

let g:lightline_buffer_readonly_icon = ''
let g:lightline_buffer_modified_icon = '✭'
" }}
" itchyny/lightline.vim {{
let g:lightline = {}
let g:lightline.tabline = {
    \   'left': [ [ 'buffers' ],
    \             [ 'separator' ],
    \             [ 'bufferbefore', 'buffercurrent', 'bufferafter' ], ],
    \   'right': [ [ 'close' ], ],
    \ }
if s:plug.is_installed('nvim-lspconfig')
    let g:lightline.component_function = {
        \   'coc_status': 'coc#status',
        \   'currentfunction': 'CocCurrentFunction',
        \   'icon_filetype': 'Get_Icon_Filetype',
        \   'icon_fileformat': 'Get_Icon_Fileformat',
        \   'branch': 'GetBranchName',
        \   'git_status': 'GetGitStatus',
        \   'filename': 'LightlineFilename',
        \   'lsp': 'LspStatus',
        \ }
else
    let g:lightline.component_function = {
        \   'coc_status': 'coc#status',
        \   'currentfunction': 'CocCurrentFunction',
        \   'icon_filetype': 'Get_Icon_Filetype',
        \   'icon_fileformat': 'Get_Icon_Fileformat',
        \   'branch': 'GetBranchName',
        \   'git_status': 'GetGitStatus',
        \   'filename': 'LightlineFilename',
        \ }
endif

let g:lightline.component = {
    \   'lineinfo': ' %3l:%-2v',
    \   'percent': '%3p%%',
    \   'percentwin': '%P',
    \   'absolutepath': '%F',
    \   'relativepath': '%f',
    \   'line': '%l',
    \ }
let g:lightline.separator = {'left': '', 'right': ''}
let g:lightline.subseparator = { 'left': '', 'right': '' }

if s:plug.is_installed('lightline-ale')
    let g:lightline#ale#indicator_checking = " "
    let g:lightline#ale#indicator_infos = "כֿ"
    let g:lightline#ale#indicator_warnings = "⚠"
    let g:lightline#ale#indicator_errors = "✘"
    let g:lightline#ale#indicator_ok = "✔"
    let g:lightline.component_expand = {
      \  'buffers': 'lightline#bufferline#buffers',
      \  'linter_checking': 'lightline#ale#checking',
      \  'linter_infos': 'lightline#ale#infos',
      \  'linter_warnings': 'lightline#ale#warnings',
      \  'linter_errors': 'lightline#ale#errors',
      \  'linter_ok': 'lightline#ale#ok',
      \ }
    let g:lightline.component_type = {
      \     'linter_checking': 'right',
      \     'linter_infos': 'right',
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'right',
      \     'buffers': 'tabsel',
      \ }

    if s:plug.is_installed('nvim-lspconfig')
        let g:lightline.active = {
            \   'left': [ ['mode', 'paste'], ['filename', 'icon_filetype'], ],
            \   'right': [
            \       [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ],
            \       ['git_status', 'branch'], ['icon_fileformat', 'percent', 'line'], ['lsp'],
            \   ],
            \ }
    else
        let g:lightline.active = {
            \   'left': [ ['mode', 'paste'], ['filename', 'icon_filetype'], ],
            \   'right': [
            \       [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ],
            \       ['git_status', 'branch'], ['icon_fileformat', 'percent', 'line'],
            \   ],
            \ }
    end
else
    let g:lightline.component_expand = {
      \  'buffers': 'lightline#bufferline#buffers',
      \ }
    let g:lightline.component_type = {
        \   'buffers': 'tabsel',
        \ }

    if s:plug.is_installed('coc.nvim')
        let g:lightline.active = {
            \   'left': [ ['mode', 'paste'], ['filename', 'icon_filetype'], ['currentfunction']  ],
            \   'right': [ ['git_status', 'branch'], ['icon_fileformat', 'percent', 'line'], ['coc_status'] ],
            \ }
    elseif s:plug.is_installed('nvim-lspconfig')
        let g:lightline.active = {
            \   'left': [ ['mode', 'paste'], ['filename', 'icon_filetype'], ['currentfunction']  ],
            \   'right': [ ['git_status', 'branch'], ['icon_fileformat', 'percent', 'line'], ['lsp'] ],
            \ }
    else
        let g:lightline.active = {
            \   'left': [ ['mode', 'paste'], ['filename', 'icon_filetype'], ['currentfunction']  ],
            \   'right': [ ['git_status', 'branch'], ['icon_fileformat', 'percent', 'line'] ],
            \ }
    end

endif
let g:lightline.colorscheme = 'tokyonight'
" Use auocmd to force lightline update.
autocmd User CocStatusChange,CocDiagnosticChange call lightline#update()
" }

function! GetGitStatus()
    return gina#component#traffic#preset("fancy") == ' ' ? '↑0 ↓0' : gina#component#traffic#preset("fancy")
endfunction

function! LightlineModified()
  if &filetype == 'help'
    return ''
  elseif &modified
    return g:lightline_buffer_modified_icon
  elseif &modifiable
    return ''
  else
    return '-'
  endif
endfunction

function! LightlineReadonly()
  if &filetype == 'help'
    return ''
  elseif &readonly
    return g:lightline_buffer_readonly_icon
  else
    return ''
  endif
endfunction

function! LightlineFilename()
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
       \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
       \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction
" }}

function! Get_Icon_Filetype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . nerdfont#find() : 'no ft') : ''
endfunction

function! Get_Icon_Fileformat()
  return winwidth(0) > 70 ? (&fileformat . ' ' . nerdfont#find()) : ''
endfunction
" }}
" scrooloose/nerdtree {{
" let g:NERDTreeShowHidden = 1
" nnoremap <leader>ft :NERDTreeToggle<CR>
" nnoremap <leader>ff :NERDTreeFind<CR>
" }}
" quick-scope {{
augroup qs_colors
  autocmd!
  autocmd ColorScheme * highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
  autocmd ColorScheme * highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline
augroup END
let g:qs_lazy_highlight = 0
let g:qs_max_chars=120
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
" }}
" fern.vim {{
function! s:init_fern() abort
    setlocal nonumber
    setlocal norelativenumber
    let b:indentLine_enabled = 0
    " Disable netrw
    let g:loaded_netrw             = 1
    let g:loaded_netrwPlugin       = 1
    let g:loaded_netrwSettings     = 1
    let g:loaded_netrwFileHandlers = 1

    " Define NERDTree like mappings
    nmap <buffer> o <Plug>(fern-action-open:edit)
    nmap <buffer> go <Plug>(fern-action-open:edit)<C-w>p
    nmap <buffer> t <Plug>(fern-action-open:tabedit)
    nmap <buffer> T <Plug>(fern-action-open:tabedit)gT
    nmap <buffer> i <Plug>(fern-action-open:split)
    nmap <buffer> gi <Plug>(fern-action-open:split)<C-w>p
    nmap <buffer> s <Plug>(fern-action-open:vsplit)
    nmap <buffer> gs <Plug>(fern-action-open:vsplit)<C-w>p

    nmap <buffer> P gg

    nmap <buffer> C <Plug>(fern-action-enter)
    nmap <buffer> u <Plug>(fern-action-leave)
    nmap <buffer> r <Plug>(fern-action-reload)
    nmap <buffer> R gg<Plug>(fern-action-reload)<C-o>
    nmap <buffer> cd <Plug>(fern-action-cd)
    nmap <buffer> CD gg<Plug>(fern-action-cd)<C-o>

    nmap <buffer> I <Plug>(fern-action-hide-toggle)

    nmap <buffer> q :<C-u>quit<CR>
    nmap <buffer><expr>
      \ <Plug>(fern-my-expand-or-collapse)
      \ fern#smart#leaf(
      \   "\<Plug>(fern-action-collapse)",
      \   "\<Plug>(fern-action-expand)",
      \   "\<Plug>(fern-action-collapse)",
      \ )
    nmap <buffer><expr>
      \ <Plug>(fern-my-expand-or-enter)
      \ fern#smart#drawer(
      \   "\<Plug>(fern-action-open-or-expand)",
      \   "\<Plug>(fern-action-open-or-enter)",
      \ )
    nmap <buffer><expr>
      \ <Plug>(fern-my-collapse-or-leave)
      \ fern#smart#drawer(
      \   "\<Plug>(fern-action-collapse)",
      \   "\<Plug>(fern-action-leave)",
      \ )
    nmap <buffer><nowait> l <Plug>(fern-my-expand-or-enter)
    nmap <buffer><nowait> h <Plug>(fern-my-collapse-or-leave)

endfunction

function! s:init_nvim_tree() abort
  let g:lua_tree_side = 'left' "left by default
  let g:lua_tree_width = 30 "30 by default
  let g:lua_tree_ignore = [ 'node_modules', '.cache', '.DS_Store' ] "empty by default
  let g:lua_tree_auto_open = 0 "0 by default, opens the tree when typing `vim $DIR` or `vim`
  let g:lua_tree_auto_close = 0 "0 by default, closes the tree when it's the last window
  let g:lua_tree_follow = 0 "0 by default, this option allows the cursor to be updated when entering a buffer
  let g:lua_tree_indent_markers = 1 "0 by default, this option shows indent markers when folders are open
  let g:lua_tree_hide_dotfiles = 0 "0 by default, this option hides files and folders starting with a dot `.`
  let g:lua_tree_git_hl = 1 "0 by default, will enable file highlight for git attributes (can be used without the icons).
  let g:lua_tree_root_folder_modifier = ':~' "This is the default. See :help filename-modifiers for more options
  let g:lua_tree_tab_open = 0 "0 by default, will open the tree when entering a new tab and the tree was previously open
  let g:lua_tree_show_icons = {
      \ 'git': 1,
      \ 'folders': 1,
      \ 'files': 1,
      \}
  "If 0, do not show the icons for one of 'git' 'folder' and 'files'
  "1 by default, notice that if 'files' is 1, it will only display
  "if nvim-web-devicons is installed and on your runtimepath

  " You can edit keybindings be defining this variable
  " You don't have to define all keys.
  " NOTE: the 'edit' key will wrap/unwrap a folder and open a file
  let g:lua_tree_bindings = {
      \ 'edit':            ['<CR>', 'o'],
      \ 'edit_vsplit':     's',
      \ 'edit_split':      'x',
      \ 'edit_tab':        't',
      \ 'toggle_ignored':  'I',
      \ 'toggle_dotfiles': 'H',
      \ 'refresh':         'R',
      \ 'preview':         '<Tab>',
      \ 'cd':              '<C-]>',
      \ 'create':          'n',
      \ 'remove':          'd',
      \ 'rename':          'r',
      \ 'cut':             'x',
      \ 'copy':            'c',
      \ 'paste':           'p',
      \ 'prev_git_item':   '[c',
      \ 'next_git_item':   ']c',
      \ }

  " Disable default mappings by plugin
  " Bindings are enable by default, disabled on any non-zero value
  " let lua_tree_disable_keybindings=1

  " default will show icon by default if no icon is provided
  " default shows no icon by default
  let g:lua_tree_icons = {
      \ 'default': '',
      \ 'symlink': '',
      \ 'git': {
      \   'unstaged': "✗",
      \   'staged': "✓",
      \   'unmerged': "",
      \   'renamed': "➜",
      \   'untracked': "★"
      \   },
      \ 'folder': {
      \   'default': "",
      \   'open': ""
      \   }
      \ }

  nnoremap <silent><leader>ft :LuaTreeToggle<CR>
  nnoremap <silent><leader>fr :LuaTreeRefresh<CR>
  nnoremap <silent><leader>ff :LuaTreeFindFile<CR>
  " LuaTreeOpen and LuaTreeClose are also available if you need them

  " a list of groups can be found at `:help lua_tree_highlight`
  highlight LuaTreeFolderIcon guibg=blue
endfunction

if s:plug.is_installed('fern.vim')
  augroup fern-custom
      autocmd! *
      autocmd FileType fern call glyph_palette#apply()
      autocmd FileType fern call s:init_fern()
  augroup END
  let g:fern#drawer_keep = v:false
  let g:fern#default_hidden = 1
  let g:fern#keepalt_on_edit = 1
  let g:fern#renderer = "nerdfont"
  let g:fern#disable_viewer_hide_cursor = 1
  nmap <silent><leader>ft :Fern . -drawer -toggle<CR>
  nmap <silent><leader>ff :Fern . -reveal=% -drawer -toggle<CR>
end

if s:plug.is_installed('nvim-tree.lua')
  call s:init_nvim_tree()
end

" }}
" vim-rooter {{
nnoremap <leader>cdr :Rooter<CR>
" }}
" vim-bookmarks {{
nmap <Leader>m <Plug>BookmarkToggle
nmap <Leader>mi <Plug>BookmarkAnnotate
nmap <Leader>ma <Plug>BookmarkShowAll
nmap <Leader>mj <Plug>BookmarkNext
nmap <Leader>mk <Plug>BookmarkPrev
nmap <Leader>md <Plug>BookmarkClear
nmap <Leader>mx <Plug>BookmarkClearAll
nmap <Leader>mk <Plug>BookmarkMoveUp
nmap <Leader>mj <Plug>BookmarkMoveDown
nmap <Leader>mg <Plug>BookmarkMoveToLine
nmap mj <Plug>BookmarkNext
nmap mk <Plug>BookmarkPrev
" }}

" mhinz/vim-startify {{
let g:startify_bookmarks = split(system('awk "{print \$2}" ~/.NERDTreeBookmarks'),'\n')
let g:startify_custom_header = [
    \ '      ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁    ░▓▓▒         ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁',
    \ '     ▕                        ▁  ░░▓▓▒▒▒     ▁▔                        ▔▏',
    \ '    ▕ ▗▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚  ░░░▓▓▓▓▓▒▒▒  ▕ ▗▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▖▒▒',
    \ '    ▕ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒ ▓▓▓▓▓▓▓▓▓▒▒ ▕ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒',
    \ '    ▕ ▝▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚ ▒▓▓▓▓▓▓▓▓▓▓▓▒▒▒ ▝▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▀▘▒',
    \ '     ▕     ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▒▒    ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▏',
    \ '      ▔▔▔▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▒  ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓    ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓    ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓   ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓    ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓    ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓    ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▒▒',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓   ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▒▓▓▒▒▒',
    \ '        ░▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓   ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▒▒▒',
    \ '       ░░▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓    ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▒▒▒',
    \ '     ░░░▓▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓    ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▒▒',
    \ '   ░░░▓▓▓▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒    ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▒▒',
    \ ' ░░░▓▓▓▓▓▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒  ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▒▒▒',
    \ '▒▒▒▓▓▓▓▓▓▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒',
    \ ' ▒▒▒▓▓▓▓▓▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓████',
    \ '   ▒▒▒▓▓▓▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓███',
    \ '     ▒▒▓▓▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▖▖▖▖▖▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓███',
    \ '      ▒▒▒▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▚▚▚▚▚▘▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓███',
    \ '       ▒▒▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒ ▚▚▚▚▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓███',
    \ '        ▒▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓███',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▚▚▚▚▚▚▚▚▓▓▓▚▚▚▚▚▚▖▓▓▗▚▚▚▚▚▖██ ▗▚▚▚▚▚',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▓▓▓▚▚▚▚▘▓▓▓▓▓▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▓▓▓▓▚▚▚▚▚▎▓▓▓▓▓▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▒▓▓▓▓▚▚▚▚▚▎▓▓▓▓▓▚▚▚▚▓▓▓▓▞▚▚▚▚▚      ▚▚▚▚▚',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▚▚▒▒▓▓▓▓▓▚▚▚▚▚▘▓▓▓▓▓▚▚▚▚▚▓▓██▞▚▚▚▚▚     ▚▚▚▚▚',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▚▚▒▒▒▒▓▓▓▓▓▚▚▚▚▚▘▓▓▓▓▚▚▚▚▚▓███  ▚▚▚▚      ▚▚▚▚▚',
    \ '         ▏ ▚▚▚▚▚▚▚▚▚▒▒▒▒▒▒▒▓▓▓▚▚▚▚▞▞▓▓▓▓▓▚▚▚▚▓██   ▚▚▚▚▚     ▚▚▚▚▚',
    \ '         ▏ ▚▚▚▚▚▚▒▒▒▒    ▒▒▒▒▚▚▚▚▚▚▓▓▓▓▓▚▚▚▚▚██    ▚▚▚▚     ▚▚▚▚▚▚',
    \ '         ▔▁▀▒▒▒▒▒▒         ▒▒▚▚▚▚▚▚▚▚▓▓▓▚▚▚▚▚▚    ▚▚▚▚▚▚    ▚▚▚▚▚▚▚',
    \ '           ▔                  ▒▒▓▓▓▓▓▓▓▓███',
    \ '                               ▒▒▒▓▓▓▓███',
    \ '                                 ▒▒▒▓██▓',
    \ '                                   ▒█▓',
    \ ]
let g:startify_files_number = 10
let g:startify_list_order = [
        \ ['♻  最近使ったファイル:'],
        \ 'files',
        \ ['♲  最近使ったファイル(カレントディレクトリ下):'],
        \ 'dir',
        \ ['⚑  セッション:'],
        \ 'sessions',
        \ ['☺  ブックマーク:'],
        \ 'bookmarks',
        \ ]
let NERDTreeHijackNetrw = 0
" }}
" hardcoreplayers/dashboard-nvim {{
" function! s:init_dashboard() abort
"     setlocal nonumber
"     setlocal norelativenumber
"     let b:indentLine_enabled = 0
" endfunction
" 
" let g:dashboard_default_executive ='fzf'
" let g:dashboard_custom_shortcut={
"      \ 'last_session'       : 'SPC s l',
"      \ 'find_history'       : 'SPC s r',
"      \ 'find_file'          : 'SPC s f',
"      \ 'change_colorscheme' : 'SPC t c',
"      \ 'find_word'          : 'SPC s g',
"      \ 'book_marks'         : 'SPC s m',
"      \ }
" nmap <Leader>ss :<C-u>SessionSave<CR>
" nmap <Leader>sl :<C-u>SessionLoad<CR>
" 
" augroup dashboard-custom
"     autocmd FileType dashboard IndentLinesDisable
" augroup END

" }}
" liuchengxu/vista.vim {{
" How each level is indented and what to prepend.
" This could make the display more compact or more spacious.
" e.g., more compact: ["▸ ", ""]
let g:vista_icon_indent = ["╰─▸ ", "├─▸ "]

" Executive used when opening vista sidebar without specifying it.
" See all the avaliable executives via `:echo g:vista#executives`.

if s:plug.is_installed('vim-lsp')
    let g:vista_default_executive = 'vim_lsp'
elseif s:plug.is_installed('nvim-lspconfig')
    let g:vista_default_executive = 'nvim_lsp'
elseif s:plug.is_installed('coc.nvim')
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
"{{{tmuxline.vim
if g:vimIsInTmux == 1
    let g:tmuxline_preset = {
                \'a'    : '#S',
                \'b'    : '%R',
                \'c'    : [ '#{sysstat_mem} #[fg=blue]\ufa51#{upload_speed}' ],
                \'win'  : [ '#I', '#W' ],
                \'cwin' : [ '#I', '#W', '#F' ],
                \'x'    : [ "#[fg=blue]#{download_speed} \uf6d9 #{sysstat_cpu}" ],
                \'y'    : [ '%a' ],
                \'z'    : '#H #{prefix_highlight}'
                \}
    let g:tmuxline_separators = {
                \ 'left' : "\ue0bc",
                \ 'left_alt': "\ue0bd",
                \ 'right' : "\ue0ba",
                \ 'right_alt' : "\ue0bd",
                \ 'space' : ' '}
endif
"}}}
nnoremap <leader>tc :Vista coc<CR>
nnoremap <leader>tt :Vista!! <CR>
" }}
" gina.vim {{
call gina#custom#mapping#nmap(
	      \ 'status', 'dd',
	      \ ':<C-u>Gina diff --opener=tabedit<CR>',
	      \ {'noremap': 1, 'silent': 1},
	      \)
call gina#custom#mapping#nmap(
	      \ 'status', 'dp',
	      \ ':<C-u>Gina diff --opener=preview<CR>',
	      \ {'noremap': 1, 'silent': 1},
	      \)
nnoremap <silent> <leader>gs :<C-u>Gina status --opener=split<CR>
nnoremap <silent> <leader>gc :<C-u>Gina commit --opener=vsplit<CR>
nnoremap <silent> <leader>gD :<C-u>Gina compare --opener=tabedit<CR>
nnoremap <silent> <leader>gd :<C-u>Gina diff --opener=tabedit<CR>
nnoremap <silent> <leader>gl :<C-u>Gina log --graph --opener=tabedit<CR>
nnoremap <silent> <leader>gb :<C-u>Gina blame --opener=vsplit<CR>
nnoremap <leader>gp :<C-u>Gina push<CR>

" }}
" vim-session {{
let g:session_autosave = 'no'
let g:session_autoload = 'no'
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
let g:mkdp_markdown_css = 'https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css'

" use a custom highlight style must absolute path
let g:mkdp_highlight_css = ''

" use a custom port to start server or random for empty
let g:mkdp_port = ''

" preview page title
" ${name} will be replace with the file name
let g:mkdp_page_title = '「${name}」'

nmap <leader>p <Plug>MarkdownPreview
" nmap <M-s> <Plug>MarkdownPreviewStop
" nmap <C-p> <Plug>MarkdownPreviewToggle
" }}
" memolist {{
let g:memolist_path = "~/Dropbox/notes"
let g:memolist_memo_suffix = "markdown"
let g:memolist_memo_date = "%Y-%m-%d %H:%M"
let g:memolist_prompt_tags = 1
let g:memolist_prompt_categories = 1
let g:memolist_fzf = 1

nnoremap <Leader>mn  :MemoNew<CR>
nnoremap <Leader>ml  :MemoList<CR>
nnoremap <Leader>mg  :MemoGrep<CR>
" }}
" undotree{{
if has("persistent_undo")
    set undodir="~/Dropbox/undodir"
    set undofile
endif
nnoremap <Leader>ut :UndotreeToggle<cr>
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
nnoremap <Leader>aj :AnyJump<CR>
" }}
" eft {{
let g:eft_ignorecase = v:true
nmap ; <Plug>(eft-repeat)
xmap ; <Plug>(eft-repeat)

nmap f <Plug>(eft-f)
xmap f <Plug>(eft-f)
omap f <Plug>(eft-f)
nmap F <Plug>(eft-F)
xmap F <Plug>(eft-F)
omap F <Plug>(eft-F)

nmap t <Plug>(eft-t)
xmap t <Plug>(eft-t)
omap t <Plug>(eft-t)
nmap T <Plug>(eft-T)
xmap T <Plug>(eft-T)
omap T <Plug>(eft-T)
let g:eft_highlight = {
    \   '1': {
    \     'highlight': 'EftChar',
    \     'allow_space': v:true,
    \     'allow_operator': v:true,
    \   },
    \   '2': {
    \     'highlight': 'EftSubChar',
    \     'allow_space': v:false,
    \     'allow_operator': v:false,
    \   },
    \   'n': {
    \     'highlight': 'EftSubChar',
    \     'allow_space': v:false,
    \     'allow_operator': v:false,
    \   }
    \ }
" }}
" vimdocs {{
let g:devdocs_filetype_map = {
    \   'typescript.jsx': 'react',
    \   'typescriptreact': 'react',
    \ }
nnoremap <Leader>d :DevDocsUnderCursor<CR>
nnoremap <Leader>da :DevDocsAll
" }}
" tyru/operator-camelize.vim {{
map <leader>c <plug>(operator-camelize)
map <leader>C <plug>(operator-decamelize)
" }}
" t9md/vim-choosewin {{
nmap <leader>w <Plug>(choosewin)
let g:choosewin_overlay_enable = 1
" }}


"*****************************************************************************
" Visual Settings
"*****************************************************************************

" Use as many color as possible
if !has('gui_running')
      \ && exists('&termguicolors')
      \ && $COLORTERM =~# '^\%(truecolor\|24bit\)$'
  " https://medium.com/@dubistkomisch/how-to-actually-get-italics-and-true-colour-to-work-in-iterm-tmux-vim-9ebe55ebc2be
" use truecolor in term
  if exists('&pumblend')
    set pumblend=20
  endif
endif

if (has("termguicolors"))
  set termguicolors
endif

" completion settings
" set complete&
"      \ complete+=k
"      \ complete+=s
"      \ complete+=i
"      \ complete+=d
"      \ complete+=t
set complete-=i
set completeopt&
      \ completeopt+=menuone
      \ completeopt+=noinsert
      \ completeopt+=noselect
      \ completeopt+=longest
      \ completeopt-=preview

if $TERM =~# '\v(xterm|tmux)-256color' || has('gui_running')
  if has('osx')
    let &t_ZH = "\e[3m"
    let &t_ZR = "\e[23m"
  endif
endif
" set t_Co=256
" let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
" let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"

set wrap
set wildmenu
set wildmode=full
set ttyfast
set lazyredraw

set guifont=FuraCode\ Nerd\ Font\ Mono:h16
set number norelativenumber
set laststatus=2 " ステータスラインを常に表示
set showmode " 現在のモードを表示
set showcmd " 打ったコマンドをステータスラインの下に表示
set noruler
set nocursorline
set hlsearch
set backspace=indent,eol,start

if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif
set display+=lastline

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

set background=dark
let g:material_theme_style='default'
let g:material_terminal_italics = 1
let g:edge_style = 'neon'
let g:edge_enable_italic = 1
let g:edge_disable_italic_comment = 1
let g:edge_current_word = 'bold'
let g:gruvbox_material_background = 'medium'
let g:gruvbox_material_enable_italic = 1
let g:gruvbox_material_disable_italic_comment = 1
let g:seoul256_background = 237
" ghifarit53/tokyonight.vim {{
let g:tokyonight_style = 'storm' " available: night, storm
let g:tokyonight_enable_italic = 1
let g:tokyonight_disable_italic_comment = 0
" }}
" joshdick/onedark.vim {{
let g:onedark_termcolors=256
let g:onedark_terminal_italics=1
let g:onedark_hide_endofbuffer=1
" }}
let g:neodark#use_256color = 1 " default: 0
let g:neodark#solid_vertsplit = 1 " default: 0

" 'drewtempelmeyer/palenight.vim'{{
let g:palenight_terminal_italics=1
" }}
" miramare {{
let g:miramare_enable_italic = 1
let g:miramare_disable_italic_comment = 1
let g:miramare_enable_bold = 1
" }}

colorscheme tokyonight
set shell=zsh

"*****************************************************************************
" Copy/Paste/Cut
"*****************************************************************************
set clipboard^=unnamed,unnamedplus


"*****************************************************************************
" Encoding
"*****************************************************************************
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8

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

autocmd BufRead,BufNewFile *.go setfiletype go
autocmd FileType go setlocal noexpandtab
autocmd FileType dart setlocal ts=2 sw=2
autocmd FileType vue setlocal ts=2 sw=2
autocmd FileType typescript setlocal ts=2 sw=2
autocmd FileType typescriptreact setlocal ts=2 sw=2
autocmd FileType javascript setlocal ts=2 sw=2

" No beep
set visualbell
set noerrorbells
" set redrawtime=10000


"*****************************************************************************
" Other
"*****************************************************************************
" if hidden not set, TextEdit might fail.
set hidden
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

set showtabline=2
" Smaller updatetime for CursorHold & CursorHoldI
" set updatetime=3000

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
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
"*****************************************************************************
" KeyMap
"*****************************************************************************

nnoremap <Leader>r :source ~/.vimrc<Enter>

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
nnoremap <Leader>bd :bdelete<Enter>
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
nnoremap <Left> :bprev<CR>
nnoremap <Right> :bnext<CR>

inoremap jj <ESC>

nnoremap L 10l
nnoremap H 10h
vnoremap L 10l
vnoremap H 10h

" terminal
" ESCでターミナルモードからノーマルモードへ
if has("nvim")
    tnoremap <C-W>N <C-\><C-N>
endif

nnoremap <Leader>cdg :cd %:h<Enter>:pwd<Enter>
nnoremap <Leader>cdl :lcd %:h<Enter>:pwd<Enter>
