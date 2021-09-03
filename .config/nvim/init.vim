scriptencoding=utf-8
set termguicolors

let mapleader = "\<Space>"

let g:use_treesitter = v:true
let g:lsp_client_type ='neovim' " neovim(builtin), coc
lua require('plugins')
augroup my_packer
  autocmd!
  autocmd BufWritePost plugins.lua :PackerCompile<CR>
augroup end

set background=dark
" colorscheme palenight
" colorscheme miramare
" colorscheme melange
" colorscheme material
" colorscheme embark
" colorscheme gruvbox-material

" colorscheme OceanicNext
" colorscheme edge
" colorscheme tokyonight
" colorscheme zephyr
" colorscheme space-nvim
colorscheme gruvbox-flat
" colorscheme doom-one


filetype plugin indent on
syntax on

if !&compatible
  set nocompatible
endif

" reset augroup
augroup vimrc
  autocmd!
augroup END

" load my scripts {{
lua require('scripts/gen_gitignore').setup()
" }}


" nvim-treesitter {{
if (g:use_treesitter)
  lua require('treesitter')
endif
" }}

" my utils {{

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
" }}

" telescope.nvim {{
function! s:init_telescope()
  " bat (preview) *
  " ripgrep (finder) *
  " Treesitter (nvim-treesitter) (finder/preview)
  " fd (sharkdp/fd) (finder)
  " git (picker) *
  " neovim LSP (picker)
  " devicons
  nnoremap <Leader>sf <cmd>lua require('telescope.builtin').git_files{}<CR>
  nnoremap <Leader>sgc <cmd>lua require('telescope.builtin').git_bcommits{}<CR>
  nnoremap <Leader>sgC <cmd>lua require('telescope.builtin').git_commits{}<CR>
  nnoremap <Leader>sgs <cmd>lua require('telescope.builtin').git_status{}<CR>
  nnoremap <Leader>sgb <cmd>lua require('telescope.builtin').git_branches{}<CR>
  nnoremap <Leader>sF <cmd>lua require('telescope.builtin').find_files{ find_command = {"rg", "-i", "--hidden", "--files", "-g", "!.git"} }<CR>
  nnoremap <Leader>s <cmd>lua require('telescope.builtin').current_buffer_fuzzy_find{}<CR>
  nnoremap <Leader>sg <cmd>lua require('telescope.builtin').live_grep{}<CR>
  nnoremap <Leader>sb <cmd>lua require('telescope.builtin').buffers{ show_all_buffers = true, generic_sorters = require('telescope.sorters').fuzzy_with_index_bias }<CR>
  nnoremap <Leader>sc <cmd>lua require('telescope.builtin').command_history{}<CR>
  nnoremap <Leader>sr <cmd>lua require('telescope.builtin').oldfiles{}<CR>
  nnoremap <Leader>sl <cmd>lua require('telescope.builtin').loclist{}<CR>
  nnoremap <Leader>sq <cmd>lua require('telescope.builtin').quickfix{}<CR>
  nnoremap <Leader>sj <cmd>lua require('telescope').extensions.jumps.jumps{}<CR>
  nnoremap <Leader>st <cmd>lua require('telescope.builtin').treesitter{}<CR>

  if g:lsp_client_type == 'neovim'
    nnoremap <silent>gr    <cmd>Telescope lsp_references<CR>
  elseif g:lsp_client_type == 'coc'
    nnoremap <Leader>sd <cmd>:Telescope coc diagnostics<CR>
    nnoremap <Leader>sD <cmd>:Telescope coc workspace_diagnostics<CR>
    nnoremap <silent>gr <cmd>:Telescope coc references<CR>
    nnoremap <silent>gy <cmd>:Telescope coc type_definitions<CR>
    nnoremap <silent>gi <cmd>:Telescope coc implementations<CR>
    nnoremap <Leader>sca <cmd>:Telescope coc code_actions<CR>
    nnoremap <Leader>ssw <cmd>:Telescope coc workspace_symbols<CR>
    nnoremap <Leader>ssd <cmd>:Telescope coc document_symbols<CR>
  endif

endfunction

call s:init_telescope()
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
let test#strategy = "neovim"
let g:test#javascript#runner = 'jest'
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
    hi! LspDiagnosticsVirtualTextError guifg=#FF0000 guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsVirtualTextWarning guifg=#FFDD00 guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsVirtualTextInformation guifg=#02DB1F guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsVirtualTextHint guifg=#02DAF2 guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsSignError guifg=#FF0000 guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsSignWarning guifg=#FFDD00 guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsSignInformation guifg=#02DB1F guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsSignHint guifg=#02DAF2 guibg=NONE guisp=NONE gui=NONE cterm=bold
    hi! LspDiagnosticsUnderlineError guifg=#FF0000 guibg=NONE guisp=#FF0000 gui=underline cterm=underline
    hi! LspDiagnosticsUnderlineWarning guifg=#FFDD00 guibg=NONE guisp=#FFDD00 gui=underline cterm=underline
    hi! LspDiagnosticsUnderlineInformation guifg=#02DB1F guibg=NONE guisp=#02DB1F gui=underline cterm=underline
    hi! LspDiagnosticsUnderlineHint guifg=#02DAF2 guibg=NONE guisp=#02DAF2 gui=underline cterm=underline
endfunction


function! s:setup_nvim_lsp()
    lua require('lsp_settings')
    call sign_define("LspDiagnosticsSignError", {"text" : "✘", "texthl" : "LspDiagnosticsSignError"})
    call sign_define("LspDiagnosticsSignWarning", {"text" : "⚠", "texthl" : "LspDiagnosticsSignWarning"})
    call sign_define("LspDiagnosticsSignInformation", {"text" : "כֿ", "texthl" : "LspDiagnosticsSignInformation"})
    call sign_define("LspDiagnosticsSignHint", {"text" : "•", "texthl" : "LspDiagnosticsSignHint"})

    " builtin mapping
    nnoremap <silent> gD    <cmd>lua vim.lsp.buf.declaration()<CR>
    nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
    nnoremap <silent> gi    <cmd>lua vim.lsp.buf.implementation()<CR>
    nnoremap <silent> gy   <cmd>lua vim.lsp.buf.type_definition()<CR>
    " nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
    nnoremap <leader>F    <cmd>lua vim.lsp.buf.formatting()<CR>

    nnoremap <silent> pd    <cmd>lua vim.lsp.buf.peek_definition()<CR>
    nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
    nnoremap <silent> H     <cmd>lua vim.lsp.buf.signature_help()<CR>
    nnoremap <leader>rn    <cmd>lua vim.lsp.buf.rename()<CR>
    nnoremap <leader>ac    <cmd>lua vim.lsp.buf.code_action()<CR>
    nnoremap <leader>dc <cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>
    nnoremap <leader>dn <cmd>lua vim.lsp.diagnostic.goto_next()<CR>
    nnoremap <leader>dp <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>
    nnoremap <leader>do <cmd>lua vim.lsp.diagnostic.set_loclist()<CR>

    " lspsaga mapping
    " lsp provider to find the currsor word definition and reference
    " nnoremap <silent> gf :Lspsaga lsp_finder<CR>
    " nnoremap <silent><leader>ac :Lspsaga code_action<CR>
    " vnoremap <silent><leader>ac :<C-u>Lspsaga range_code_action<CR>
    " nnoremap <leader>rn :Lspsaga rename<CR>
    " nnoremap <silent>K :Lspsaga hover_doc<CR>
    " nnoremap <silent> <C-f> <cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>
    " nnoremap <silent> <C-b> <cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>
    " nnoremap <silent> H :Lspsaga signature_help<CR>
    " nnoremap <silent> gdp :Lspsaga preview_definition<CR>
    " nnoremap <silent> <leader>dp :Lspsaga diagnostic_jump_prev<CR>
    " nnoremap <silent> <leader>dn :Lspsaga diagnostic_jump_next<CR>
    " nnoremap <silent> <leader>dc :Lspsaga show_line_diagnostics<CR>

endfunction

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
  call s:setup_nvim_lsp()
  call s:setup_vsnip()
elseif g:lsp_client_type == 'coc'
  call s:setup_coc()
endif
" vim-json {{
let g:vim_json_syntax_conceal = 0
" }}

" blamer.nvim{{
let g:blamer_enabled = 0
let g:blamer_delay = 1000
let g:blamer_show_in_visual_modes = 1
let g:blamer_prefix = '  '
" Available options: <author>, <author-mail>, <author-time>, <committer>, <committer-mail>, <committer-time>, <summary>, <commit-short>, <commit-long>.
let g:blamer_template = '<committer>, <committer-time> • <summary>'
nnoremap <Leader>gbt :BlamerToggle<CR>
" }}

" gitsigns.nvim {{
lua << EOF
require('gitsigns').setup{
  signs = {
    add          = {hl = 'GitGutterAdd'   , text = '┃'},
    change       = {hl = 'GitGutterChange', text = '┃'},
    delete       = {hl = 'GitGutterDelete', text = '┃'},
    topdelete    = {hl = 'GitGutterDelete', text = '┃'},
    changedelete = {hl = 'GitGutterChange', text = '┃'},
  },
  sign_priority = 1,
}
EOF

" }}


" nvim-colorizer.lua{{
lua require('colorizer').setup()
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
" gina.vim {{
call gina#custom#mapping#nmap(
	      \ 'status', 'dd',
	      \ ':<C-u>Gina diff --opener=vsplit<CR>',
	      \ {'noremap': 1, 'silent': 1},
	      \)
call gina#custom#mapping#nmap(
	      \ 'status', 'dp',
	      \ ':<C-u>Gina diff --opener=preview<CR>',
	      \ {'noremap': 1, 'silent': 1},
	      \)
nnoremap <leader>gs :<C-u>Gina status --opener=split<CR>
nnoremap <leader>gc :<C-u>Gina commit --opener=vsplit<CR>
nnoremap <leader>gD :<C-u>Gina compare --opener=tabedit<CR>
nnoremap <leader>gd :<C-u>Gina diff --opener=tabedit<CR>
nnoremap <leader>gl :<C-u>Gina log --graph --opener=tabedit<CR>
nnoremap <leader>gb :<C-u>Gina blame --opener=vsplit<CR>
vnoremap <leader>gln :<C-u>:'<,'>Gina browse --exact --yank :<CR>
nnoremap <leader>gln :<C-u>:Gina browse --exact --yank :<CR>
nnoremap <leader>gm :<C-u>:Gina chaperon<CR>
nnoremap <leader>gp :<C-u>Gina push<CR>

" }}
" git-messanger {{
let g:git_messenger_always_into_popup = v:true
let g:git_messenger_include_diff = "current"
nmap <Leader>gm <Plug>(git-messenger)
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
let g:memolist_memo_suffix = "md"
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
nnoremap <Leader>j :AnyJump<CR>
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

" wilder.nvim {{
" call wilder#enable_cmdline_enter()
" set wildcharm=<Tab>
" cmap <expr> <Tab> wilder#in_context() ? wilder#next() : "\<Tab>"
" cmap <expr> <S-Tab> wilder#in_context() ? wilder#previous() : "\<S-Tab>"
" call wilder#set_option('renderer', wilder#popupmenu_renderer({
"       \ 'highlighter': wilder#basic_highlighter(),
"       \ 'left': [
"       \   wilder#popupmenu_devicons(),
"       \ ],
"       \ }))

" only / and ? are enabled by default
" call wilder#set_option('modes', ['/', '?', ':'])
" }}
" bfredl/nvim-miniyank {{
map p <Plug>(miniyank-autoput)
map P <Plug>(miniyank-autoPut)
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
" Visual Settings
"*****************************************************************************

au TextYankPost * silent! lua vim.highlight.on_yank {timeout = 300}

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

" completion settings
" set complete&
"      \ complete+=k
"      \ complete+=s
"      \ complete+=i
"      \ complete+=d
"      \ complete+=t
set complete&
    \ complete-=i
    \ complete-=t
set completeopt&
      \ completeopt+=menuone
      \ completeopt+=noselect
      \ completeopt-=preview

if $TERM =~# '\v(xterm|tmux)-256color' || has('gui_running')
  if has('osx')
    let &t_ZH = "\e[3m"
    let &t_ZR = "\e[23m"
  endif
endif
set t_Co=256
let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"


set wrap
set wildmenu
set wildmode=full
set ttyfast
set lazyredraw
" set virtualedit=all
set number norelativenumber
set laststatus=2 " ステータスラインを常に表示
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

let g:material_theme_style='default'
let g:material_terminal_italics = 1
let g:edge_style = 'aura'
let g:edge_enable_italic = 1
let g:edge_disable_italic_comment = 0
let g:edge_current_word = 'bold'
let g:gruvbox_material_background = 'medium'
let g:gruvbox_material_enable_italic = 1
let g:gruvbox_material_disable_italic_comment = 1
let g:seoul256_background = 237
" tokyonight.nvim {{
let g:tokyonight_style = 'storm' " available: night, storm
let g:tokyonight_italic_comment = 1
let g:tokyonight_italic_keywords = 1
let g:tokyonight_italic_functions = 1
let g:tokyonight_transparent = 0
let g:tokyonight_hide_inactive_statusline = 1
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

" oceanic-next{{
let g:oceanic_next_terminal_bold = 1
let g:oceanic_next_terminal_italic = 1
" }}

" embark{{
let g:embark_terminal_italics = 1
" }}

set shell=zsh
set mouse=n

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
autocmd FileType vim setlocal ts=2 sw=2
autocmd FileType lua setlocal ts=2 sw=2
autocmd FileType yaml setlocal ts=2 sw=2
autocmd FileType python setlocal ts=4 sw=4
autocmd FileType proto setlocal ts=2 sw=2

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

" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=4000

" don't give |ins-completion-menu| messages.
set shortmess&
    \ shortmess+=c
    \ shortmess-=S

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("nvim-0.5.0") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

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

