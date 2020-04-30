if !&compatible
  set nocompatible
endif

if exists('g:vscode')
  let mapleader = "\<Space>"

  nnoremap <silent> <leader>qp <Cmd>call VSCodeCall('workbench.action.closePanel')<CR>
  nnoremap <silent> <leader>q <Cmd>call VSCodeCall('workbench.action.closeActiveEditor')<CR>
  nnoremap <silent> <leader>ft <Cmd>call VSCodeCall('workbench.action.toggleSidebarVisibility')<CR>
  nnoremap <silent> - <Cmd>call VSCodeCall('workbench.files.action.showActiveFileInExplorer')<CR>
  nnoremap <silent> <leader>p <Cmd>call VSCodeCall('workbench.action.showCommands')<CR>
  nnoremap <silent> <leader>sf <Cmd>call VSCodeCall('workbench.action.quickOpen')<CR>
  nnoremap <silent> <leader>sp <Cmd>call VSCodeCall('projectManager.listProjects')<CR>
  nnoremap <silent> <leader>tt <Cmd>call VSCodeCall('workbench.action.terminal.focus')<CR>
  nnoremap <silent> <leader>gs <Cmd>call VSCodeCall('workbench.view.scm')<CR>
  nnoremap <silent> <leader>rn <Cmd>call VSCodeCall('editor.action.rename')<CR>
  nnoremap <silent> K <Cmd>call VSCodeCall('editor.action.showHover')<CR>
  nnoremap <silent> gD <Cmd>call VSCodeCall('editor.action.goToImplementation')<CR>
  nnoremap <silent> gd <Cmd>call VSCodeCall('editor.action.goToDeclaration')<CR>
  nnoremap <silent> gr <Cmd>call VSCodeCall('references-view.find')<CR>
  nnoremap <silent> gR <Cmd>call VSCodeCall('references-view.findImplementations')<CR>
  nnoremap <silent> <delete> <Cmd>call VSCodeCall('editor.debug.action.toggleBreakpoint')<CR>
  " nnoremap <silent> gO <Cmd>call VSCodeCall('workbench.action.gotoSymbol')<CR>
  nnoremap <silent> gO <Cmd>call VSCodeCall('outline.focus')<CR>
  nnoremap <silent> z/ <Cmd>call VSCodeCall('workbench.action.showAllSymbols')<CR>
  nnoremap <silent> <c-b> <Cmd>call VSCodeCall('workbench.action.showAllEditorsByMostRecentlyUsed')<CR>

  nnoremap <silent> UD <Cmd>call VSCodeCall('git.openChange')<CR>
  nnoremap <silent> UW <Cmd>call VSCodeCall('git.stage')<CR>
  nnoremap <silent> UB <Cmd>call VSCodeCall('gitlens.toggleFileBlame')<CR>
  xmap gc  <Plug>VSCodeCommentary
  nmap gc  <Plug>VSCodeCommentary
  omap gc  <Plug>VSCodeCommentary
  nmap gcc <Plug>VSCodeCommentaryLine
  finish
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

" Plug 'junegunn/fzf', { 'do': './install --all' }
"   Plug 'junegunn/fzf.vim'
Plug 'Yggdroot/LeaderF', { 'do': './install.sh' }
Plug 'luochen1990/rainbow'
" <leader>qでアクティブなBufferをキル（windowはそのまま）
Plug 'moll/vim-bbye'
" gcでコメントアウト
Plug 'tpope/vim-commentary'
Plug 'thinca/vim-quickrun'
Plug 'godlygeek/tabular'
" Languages
Plug 'plasticboy/vim-markdown', {'for': 'markdown'}
Plug 'thosakwe/vim-flutter'
Plug 'sheerun/vim-polyglot'
Plug 'metakirby5/codi.vim'

" Completion
if has('nvim')
    " use coc.nvim
    Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
    " use neovim built-in
    " Plug 'neovim/nvim-lsp'
    " Plug 'h-michael/lsp-ext.nvim'
    " Plug 'haorenW1025/completion-nvim'
    " use vim-lsp
    " Plug 'prabirshrestha/vim-lsp'
    " Plug 'mattn/vim-lsp-settings'
    " Plug 'prabirshrestha/asyncomplete-lsp.vim'
    " use asyncomplete
    " Plug 'prabirshrestha/async.vim'
    " Plug 'prabirshrestha/asyncomplete-buffer.vim'
    " Plug 'prabirshrestha/asyncomplete-file.vim'
    " Plug 'prabirshrestha/asyncomplete.vim'
    " use ale
    " Plug 'dense-analysis/ale'
    " Plug 'maximbaz/lightline-ale'
else
    " use coc.nvim
    Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
    " use vim-lsp
    " Plug 'prabirshrestha/async.vim'
    " Plug 'prabirshrestha/vim-lsp'
    " Plug 'mattn/vim-lsp-settings'
    " Plug 'dense-analysis/ale'
    " Plug 'maximbaz/lightline-ale'
    " use deoplete
    " Plug 'roxma/nvim-yarp'
    " Plug 'roxma/vim-hug-neovim-rpc'
    " Plug 'Shougo/deoplete.nvim'
    " Plug 'lighttiger2505/deoplete-vim-lsp'
    " Plug 'Shougo/echodoc.vim'
    " use asyncomplete
    " Plug 'prabirshrestha/asyncomplete.vim'
    " Plug 'prabirshrestha/asyncomplete-lsp.vim'
    " Plug 'prabirshrestha/asyncomplete-buffer.vim'
    " Plug 'prabirshrestha/asyncomplete-file.vim'
endif

" Visual
Plug 'yggdroot/indentline'
Plug 'itchyny/lightline.vim'
Plug 'mengelbrecht/lightline-bufferline'
Plug 'ryanoasis/vim-devicons'
Plug 'mhinz/vim-startify'
Plug 'liuchengxu/vista.vim'
Plug 'jeffkreeftmeijer/vim-numbertoggle'

" Explorer
Plug 'preservim/nerdtree'
Plug 'airblade/vim-rooter'

" Util
Plug 'MattesGroeger/vim-bookmarks'
Plug 'unblevable/quick-scope'
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

" Git
Plug 'lambdalisue/gina.vim'
Plug 'airblade/vim-gitgutter'
Plug 'gotchane/vim-git-commit-prefix'

" MyPlug
Plug 'tkmpypy/eztrans.vim'
call plug#end()

let s:plug = get(g:, 'plugs', {})

function! s:plug.is_installed(name)
  return has_key(s:plug, a:name) ? isdirectory(s:plug[a:name].dir) : 0
endfunction

let mapleader = "\<Space>"
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
" }}
" polyglot {{
let g:polyglot_disabled = ['markdown','md']
" }}
" vim-markdown {{
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_conceal = 0
let g:vim_markdown_new_list_item_indent = 2
let g:vim_markdown_conceal_code_blocks = 0
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

function! s:setup_nvim_lsp()
    " use omnifunc
    " lua require'nvim_lsp'.vimls.setup{}
    " lua require'nvim_lsp'.jsonls.setup{}
    " lua require'nvim_lsp'.tsserver.setup{}
    " lua require'nvim_lsp'.pyls_ms.setup{}
    " lua require'nvim_lsp'.rust_analyzer.setup{}
    " autocmd Filetype vim setlocal omnifunc=v:lua.vim.lsp.omnifunc
    " autocmd Filetype typescript setlocal omnifunc=v:lua.vim.lsp.omnifunc
    " autocmd Filetype typescriptreact setlocal omnifunc=v:lua.vim.lsp.omnifunc
    " autocmd Filetype python setlocal omnifunc=v:lua.vim.lsp.omnifunc
    " autocmd Filetype json setlocal omnifunc=v:lua.vim.lsp.omnifunc
    " autocmd Filetype rust setlocal omnifunc=v:lua.vim.lsp.omnifunc

    nnoremap <silent> gD    <cmd>lua vim.lsp.buf.declaration()<CR>
    nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
    nnoremap <silent> pd    <cmd>lua vim.lsp.buf.peek_definition()<CR>
    nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
    nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
    nnoremap <silent> H     <cmd>lua vim.lsp.buf.signature_help()<CR>
    nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
    nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
    nnoremap <silent> rn    <cmd>lua vim.lsp.buf.rename()<CR>
endfunction

function! s:setup_complete_nvim()
    lua require'nvim_lsp'.pyls_ms.setup{on_attach=require'completion'.on_attach}
    lua require'nvim_lsp'.vimls.setup{on_attach=require'completion'.on_attach}
    lua require'nvim_lsp'.jsonls.setup{on_attach=require'completion'.on_attach}
    lua require'nvim_lsp'.tsserver.setup{on_attach=require'completion'.on_attach}
    lua require'nvim_lsp'.rust_analyzer.setup{on_attach=require'completion'.on_attach}

    let g:completion_enable_auto_hover = 1
    let g:completion_enable_auto_signature = 1
    let g:completion_max_items = 20
    let g:completion_trigger_character = ['.', '::']
endfunction

function! s:setup_vim_lsp()
    let g:lsp_settings_filetype_python = 'pyls-ms'
    let g:lsp_diagnostics_enabled = 1
    let g:lsp_signs_enabled = 1         " enable signs
    let g:lsp_diagnostics_echo_cursor = 1 " enable echo under cursor when in normal mode
    let g:lsp_highlights_enabled = 0
    let g:lsp_textprop_enabled = 0
    let g:lsp_highlight_references_enabled = 0
    highlight lspReference ctermfg=red guifg=red ctermbg=green guibg=green

    let g:lsp_signs_error = {'text': '✗'}
    let g:lsp_signs_warning = {'text': '‼'} " icons require GUI
    let g:lsp_signs_hint = {'test': '?'} " icons require GUI
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
          \, 'coc-gitignore'
          \ ]
    function! CocCurrentFunction()
        let funcName = get(b:, 'coc_current_function', '')
        if funcName != ''
            let funcName = ' ' . funcName
        endif
        return funcName
    endfunction

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

function! s:setup_deoplete()
    let g:deoplete#enable_at_startup = 1
    let g:echodoc#enable_at_startup = 1
    if has("nvim")
        let g:echodoc#type = "floating"
    else
        let g:echodoc#type = "popup"
    endif
    highlight link EchoDocPopup Pmenu
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
        \ 'priority': 200,
        \ 'completor': function('asyncomplete#sources#buffer#completor'),
        \ 'config': {
        \    'max_buffer_size': 500,
        \  },
        \ }))
    " file
    au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
        \ 'name': 'file',
        \ 'whitelist': ['*'],
        \ 'priority': 300,
        \ 'config': {
        \    'max_buffer_size': 500,
        \  },
        \ 'completor': function('asyncomplete#sources#file#completor')
        \ }))
    if s:plug.is_installed('asyncomplete_neovim_lsp')
        au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#lsp#get_source_options({}))
    endif
endfunction

if s:plug.is_installed('nvim-lsp')
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
        elseif s:plug.is_installed('deoplete.nvim')
            call s:setup_deoplete()
        endif

        if executable('efm-langserver') && !s:plug.is_installed('ale')
            augroup LspEFM
              au!
              autocmd User lsp_setup call lsp#register_server({
                  \ 'name': 'efm-langserver',
                  \ 'cmd': {server_info->['efm-langserver', '-c=' . $HOME . '/.config/efm-langserver/config.yaml']},
                  \ 'whitelist': ['vim', 'eruby', 'markdown', 'yaml', 'python', 'typescript', 'typescriptreact', 'javascript', 'javascriptreact'],
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
    \}
    let g:ale_linters = {
    \   '*': ['remove_trailing_lines', 'trim_whitespace'],
    \   'python': ['flake8'],
    \   'typescript': ['eslint'],
    \   'typescriptreact': ['eslint'],
    \   'javascript': ['eslint'],
    \   'javascriptreact': ['eslint'],
    \   'rust': ['rls'],
    \   'go': ['golint', 'govet', 'gofmt'],
    \   'vim': ['vint'],
    \}
    let g:ale_rust_rls_config = {
        \ 'rust': {
            \ 'all_targets': 1,
            \ 'build_on_save': 1,
            \ 'clippy_preference': 'on'
        \ }
	\ }
    let g:ale_rust_rls_toolchain = 'stable'
    let g:ale_rust_rls_executable = 'rust-analyzer'
    let g:ale_linters_explicit = 1
    let g:ale_sign_error = '✗'
    let g:ale_sign_warning = '⚠'
    let g:ale_set_highlights = 0
    let g:ale_echo_msg_error_str = 'E'
    let g:ale_echo_msg_warning_str = 'W'
    let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
    " Write this in your vimrc file
    let g:ale_lint_on_text_changed = 0
    let g:ale_lint_on_insert_leave = 0
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
" taohexxx/lightline-buffer {{
let g:lightline#bufferline#enable_devicons = 1
let g:lightline#bufferline#show_number  = 2
let g:lightline#bufferline#shorten_path = 0
let g:lightline#bufferline#unnamed      = '[No Name]'
let g:lightline#bufferline#filename_modifier = ':t'
let g:lightline#bufferline#unicode_symbols = 1

let g:lightline_buffer_readonly_icon = ''
let g:lightline_buffer_modified_icon = '✭'
" }}
" itchyny/lightline.vim {{
set showtabline=2  " always show tabline
let g:lightline = {}
let g:lightline.tabline = {
    \   'left': [ [ 'buffers' ],
    \             [ 'separator' ],
    \             [ 'bufferbefore', 'buffercurrent', 'bufferafter' ], ],
    \   'right': [ [ 'close' ], ],
    \ }
let g:lightline.component_function = {
    \   'coc_status': 'coc#status',
    \   'currentfunction': 'CocCurrentFunction',
    \   'devicons_filetype': 'Devicons_Filetype',
    \   'devicons_fileformat': 'Devicons_Fileformat',
    \   'branch': 'GetBranchName',
    \   'git_status': 'GetGitStatus',
    \   'filename': 'LightlineFilename',
    \ }
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
    let g:lightline#ale#indicator_checking = "\uf110"
    let g:lightline#ale#indicator_infos = "\uf129"
    let g:lightline#ale#indicator_warnings = "\uf071"
    let g:lightline#ale#indicator_errors = "\uf05e"
    let g:lightline#ale#indicator_ok = "\uf00c"
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
    let g:lightline.active = {
        \   'left': [ ['mode', 'paste'], ['filename', 'devicons_filetype'], ],
        \   'right': [
        \       [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ],
        \       ['git_status', 'branch'], ['devicons_fileformat', 'percent', 'line'],
        \   ],
        \ }
else
    let g:lightline.component_expand = {
      \  'buffers': 'lightline#bufferline#buffers',
      \ }
    let g:lightline.component_type = {
        \   'buffers': 'tabsel',
        \ }
    let g:lightline.active = {
        \   'left': [ ['mode', 'paste'], ['filename', 'devicons_filetype'], ['currentfunction']  ],
        \   'right': [ ['git_status', 'branch'], ['devicons_fileformat', 'percent', 'line'], ['coc_status'] ],
        \ }
endif
let g:lightline.colorscheme = 'edge'
" Use auocmd to force lightline update.
" autocmd BufWritePost,TextChanged,TextChangedI * call lightline#update()
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
" ryanoasis/vim-devicons {{
function! Devicons_Filetype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! Devicons_Fileformat()
  return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction
let g:webdevicons_enable_nerdtree = 1
let g:webdevicons_conceal_nerdtree_brackets = 1
" }}
" scrooloose/nerdtree {{
let g:NERDTreeShowHidden = 1
nnoremap <leader>ft :NERDTreeToggle<CR>
nnoremap <leader>ff :NERDTreeFind<CR>
" }}
" quick-scope {{
let g:qs_lazy_highlight = 1
let g:qs_max_chars=80
let g:qs_highlight_on_keys = ['f', 'F']
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
" tpope/vim-markdown {{
let g:vim_markdown_conceal = 0
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
elseif s:plug.is_installed('nvim_lsp')
    let g:vista_default_executive = 'vim_lsp'
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

" The default icons can't be suitable for all the filetypes, you can extend it as you wish.
let g:vista#renderer#icons = {
\   "function": "\uf794",
\   "variable": "\uf71b",
\  }
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
" joshdick/onedark.vim {{
" let g:onedark_termcolors=256
" let g:onedark_terminal_italics=1
" let g:onedark_hide_endofbuffer=1
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
let g:mkdp_auto_close = 1

" set to 1, the vim will refresh markdown when save the buffer or
" leave from insert mode, default 0 is auto refresh markdown as you edit or
" move the cursor
" default: 0
let g:mkdp_refresh_slow = 0

" set to 1, the MarkdownPreview command can be use for all files,
" by default it can be use in markdown file
" default: 0
let g:mkdp_command_for_global = 0

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
" let g:memolist_fzf = 1
let g:memolist_ex_cmd = 'Clap notes'

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
" eztrans.vim {{

" カーソル下の単語をハイライト付きで置換
nnoremap <Leader>et :EztransDefaultCurrent en ja<CR>
vnoremap <Leader>et :EztransSelection en ja<CR>

" }}
" pechorin/any-jump.vim{{
nnoremap <Leader>aj :AnyJump<CR>
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
" LeaderF {{
" don't show the help in normal mode
let g:Lf_HideHelp = 1
let g:Lf_UseCache = 0
let g:Lf_UseVersionControlTool = 0
let g:Lf_IgnoreCurrentBufferName = 1
" popup mode
let g:Lf_WindowPosition = 'popup'
let g:Lf_PreviewInPopup = 1
let g:Lf_StlSeparator = { 'left': "\ue0b0", 'right': "\ue0b2", 'font': "DejaVu Sans Mono for Powerline" }
let g:Lf_PreviewResult = {'Function': 0, 'BufTag': 0 }

let g:Lf_ShortcutF = "<leader>sf"
let g:Lf_ShowHidden = 1
let g:Lf_RgConfig = [
    \ "--glob=!git/*",
    \ "--hidden",
    \ "-S",
    \ "--line-number",
    \ "--no-heading",
    \ "--smart-case",
\ ]
noremap <leader>sb :<C-U><C-R>=printf("Leaderf buffer %s", "")<CR><CR>
noremap <leader>sr :<C-U><C-R>=printf("Leaderf mru %s", "")<CR><CR>
noremap <leader>st :<C-U><C-R>=printf("Leaderf bufTag %s", "")<CR><CR>
noremap <leader>sl :<C-U><C-R>=printf("Leaderf line %s", "")<CR><CR>

noremap <leader>sgb :<C-U><C-R>=printf("Leaderf! rg --current-buffer -e %s ", expand("<cword>"))<CR>
noremap <leader>sgc :<C-U><C-R>=printf("Leaderf rg -e %s ", expand("<cword>"))<CR><CR>
noremap <leader>sg :<C-U><C-R>=printf("Leaderf rg %s", "")<CR><CR>
" search visually selected text literally
" xnoremap <leader>sg :<C-U><C-R>=printf("Leaderf! rg -F -e %s ", leaderf#Rg#visual())<CR>
noremap <leader>sR :<C-U>Leaderf --recall<CR>

" should use `Leaderf gtags --update` first
let g:Lf_GtagsAutoGenerate = 0
let g:Lf_Gtagslabel = 'native-pygments'
let g:Lf_ShowDevIcons = 1
" }}
"*****************************************************************************
" Visual Settings
"*****************************************************************************
" Use as many color as possible
if !has('gui_running')
      \ && exists('&termguicolors')
      \ && $COLORTERM =~# '^\%(truecolor\|24bit\)$'
  " https://medium.com/@dubistkomisch/how-to-actually-get-italics-and-true-colour-to-work-in-iterm-tmux-vim-9ebe55ebc2be
  if !has('nvim')
    let &t_8f = "\e[38;2;%lu;%lu;%lum"
    let &t_8b = "\e[48;2;%lu;%lu;%lum"
  endif
  set termguicolors       " use truecolor in term
  if exists('&pumblend')
    set pumblend=20
  endif
endif
set termguicolors       " use truecolor in term
" for kitty
" https://sw.kovidgoyal.net/kitty/faq.html#id3
let &t_ut=''

" completion settings
set complete&
      \ complete+=k
      \ complete+=s
      \ complete+=i
      \ complete+=d
      \ complete+=t
set completeopt&
      \ completeopt+=preview
      \ completeopt+=menu
      \ completeopt+=longest

set t_Co=256

filetype plugin indent on
syntax on
set wildmenu
set wildmode=full
set ttyfast
set lazyredraw

set guifont=FuraCode\ Nerd\ Font\ Mono:h16
set number relativenumber
set laststatus=2 " ステータスラインを常に表示
set showmode " 現在のモードを表示
set showcmd " 打ったコマンドをステータスラインの下に表示
set noruler
set nocursorline
" lazy drawing
" set lazyredraw
" set ttyfast
set hlsearch
set backspace=indent,eol,start

" 引数なしでvimを開くとNERDTreeを起動
" let file_name = expand('%')
" if has('vim_starting') &&  file_name == ''
"   autocmd VimEnter * NERDTree ./
" endif

set background=dark
let g:material_theme_style='palenight'
let g:edge_style = 'neon'
let g:edge_disable_italic_comment = 1
let g:gruvbox_material_background = 'soft'
let g:seoul256_background = 237
colorscheme edge
set shell=zsh

"*****************************************************************************
" Copy/Paste/Cut
"*****************************************************************************
set clipboard=unnamed,unnamedplus


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

autocmd FileType go setlocal noexpandtab
autocmd FileType dart setlocal ts=2 sw=2
autocmd FileType vue setlocal ts=2 sw=2
autocmd FileType typescript setlocal ts=2 sw=2
autocmd FileType typescriptreact setlocal ts=2 sw=2
autocmd FileType javascript setlocal ts=2 sw=2

" No beep
set visualbell
set noerrorbells
set redrawtime=10000


"*****************************************************************************
" Other
"*****************************************************************************
" if hidden not set, TextEdit might fail.
set hidden
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes
set incsearch                                    " サーチ：インクリメンタルサーチ（検索中に文字を打つと自動で検索していく）
set ignorecase                                   " サーチ：大文字小文字を区別しない
set smartcase                                    " サーチ：大文字で検索されたら対象を大文字限定にする
set showmatch                                    " カーソル：括弧にカーソルを合わせた時、対応した括弧を表示する
set nowrap
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

" terminal
" ESCでターミナルモードからノーマルモードへ
" tnoremap <C-[> <C-\><C-n>

nnoremap <Leader>cdg :cd %:h<Enter>:pwd<Enter>
nnoremap <Leader>cdl :lcd %:h<Enter>:pwd<Enter>
