local g_var = vim.g
local opt = vim.opt
local b = vim.bo

if vim.loader then
  vim.loader.enable()
end

g_var.use_treesitter = true
g_var.lsp_client_type = "neovim" --neovim(builtin), coc
g_var.git_client_type = "neogit" --neogit, ??????
g_var.file_explorer_type = "neo-tree" -- neo-tree, nvim-tree
g_var.splash_type = "alpha" -- dashboard, alpha
g_var.test_runner_type = "neotest" -- vim-test, neotest

g_var.mapleader = " "
g_var.did_install_default_menus = 1
g_var.did_install_syntax_menu = 1
g_var.did_indent_on = 1
g_var.did_load_ftplugin = 1
g_var.loaded_2html_plugin = 1
g_var.loaded_gzip = 1
g_var.loaded_man = 1
g_var.loaded_matchit = 1
g_var.loaded_matchparen = 1
g_var.loaded_netrw = 1
g_var.loaded_netrwPlugin = 1
g_var.loaded_remote_plugins = 1
g_var.loaded_shada_plugin = 1
g_var.loaded_spellfile_plugin = 1
g_var.loaded_tarPlugin = 1
g_var.loaded_tutor_mode_plugin = 1
g_var.loaded_zipPlugin = 1
g_var.skip_loading_mswin = 1
g_var.omni_sql_no_default_maps = 1

vim.cmd([[
  " You might have to force true color when using regular vim inside tmux as the
  " colorscheme can appear to be grayscale with "termguicolors" option enabled.
  if !has('gui_running') && &term =~ '^\%(screen\|tmux\)'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  endif
  " let &t_Cs = "\e[4:3m"
  " let &t_Ce = "\e[4:0m"
]])

-- o.syntax = "on"
-- o.filetype = "on"
-- o.plugin = "on"
-- o.indent = "on"
opt.termguicolors = true
opt.shell = "zsh"
opt.mouse = "n"
opt.laststatus = 3
opt.background = "dark"
opt.splitkeep = "screen"
opt.clipboard = { "unnamed", "unnamedplus" }
opt.encoding = "utf-8"
opt.fileencodings = "utf-8"

opt.complete:remove({ "i", "t" })
opt.completeopt = { "menu", "menuone", "noselect" }
opt.ttyfast = true
opt.redrawtime = 500
opt.updatetime = 500
opt.wrap = false
opt.wildmenu = true
opt.wildmode = "full"
opt.number = true
opt.relativenumber = false
opt.showmode = true
opt.showcmd = true
opt.ruler = false
opt.cursorline = true
opt.cursorcolumn = false
opt.hlsearch = true
opt.backspace = { "indent", "eol", "start" }

if opt.scrolloff == 0 then
  opt.scrolloff = 1
end

if opt.sidescrolloff == 0 then
  opt.sidescrolloff = 5
end

if opt.compatible then
  opt.compatible = false
end

require('plugins_lazy')

opt.display:append("lastline")
opt.list = true
opt.listchars = {
  tab = "»\\",
  trail = "░",
  eol = "↲",
  extends = "☛",
  precedes = "☚",
  nbsp = "␣",
  conceal = "┊",
}
opt.visualbell = true
opt.errorbells = false
opt.expandtab = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.softtabstop = 4
opt.autoindent = true
opt.smartindent = true
opt.smarttab = true
opt.hidden = true
opt.backup = false
opt.writebackup = false
opt.cmdheight = 1
opt.shortmess = "acTSFI"
opt.signcolumn = "yes"
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true
opt.showmatch = true
opt.swapfile = false
opt.autoread = true
opt.synmaxcol = 200

vim.api.nvim_create_autocmd({ "TextYankPost" }, {
  pattern = "*",
  callback = function(_)
    vim.highlight.on_yank({ timeout = 300 })
  end,
})
vim.api.nvim_create_autocmd({ "Syntax" }, {
  pattern = "*",
  callback = function(_)
    vim.cmd([[
      syn sync minlines=500 maxlines=1000
    ]])
  end,
})
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = {
    "bash",
    "sh",
    "zsh",
    "dart",
    "vue",
    "typescript",
    "typescriptreact",
    "javascript",
    "javascriptreact",
    "lua",
    "yaml",
    "json",
    "proto",
    "markdown",
    "vim",
  },
  callback = function(_)
    b.ts = 2
    b.sw = 2
  end,
})
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = "go",
  callback = function(_)
    b.expandtab = true
  end,
})
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = "python",
  callback = function(_)
    b.ts = 4
    b.sw = 4
  end,
})

local visual_search = function()
  vim.cmd([[
    let reg = '"'
    let [save_reg, save_type] = [getreg(reg), getregtype(reg)]
    normal! gv""y
    let text = @"
    call setreg(reg, save_reg, save_type)

    let @/ = text
    call histadd('/', text)
  ]])
end

local register_path_relative = function()
  vim.cmd([[
    let @* = expand('%')
  ]])
end

local register_path_absolute = function()
  vim.cmd([[
    let @* = expand('%:p')
  ]])
end

local register_path_filename = function()
  vim.cmd([[
    let @* = expand('%:t')
  ]])
end

-- keymaps
vim.keymap.set("n", "<leader>pp", register_path_relative, { noremap = true, silent = false, desc = "Register path relative" })
vim.keymap.set("n", "<leader>pP", register_path_absolute, { noremap = true, silent = false, desc = "Register path absolute" })
vim.keymap.set("n", "<leader>pf", register_path_filename, { noremap = true, silent = false, desc = "Register path filename" })
vim.keymap.set("n", "<leader>ec", function ()
  vim.fn.execute('echo expand("%:p")', false)
end, { noremap = true, silent = true, desc = "echo current file path" })

vim.keymap.set("n", "<leader>wmh", "<C-w>H", { noremap = true, silent = true, desc = "Move window H" })
vim.keymap.set("n", "<leader>wmj", "<C-w>J", { noremap = true, silent = true, desc = "Move window J" })
vim.keymap.set("n", "<leader>wmk", "<C-w>K", { noremap = true, silent = true, desc = "Move window K" })
vim.keymap.set("n", "<leader>wml", "<C-w>L", { noremap = true, silent = true, desc = "Move window L" })
vim.keymap.set("n", "<leader>bn", function ()
  vim.fn.execute('bnext')
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>bp", function ()
  vim.fn.execute('bprevious')
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>bra", function ()
  vim.fn.execute('bufdo e!')
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>tn", function ()
  vim.fn.execute('tabn')
end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>tp", function ()
  vim.fn.execute('tabp')
end, { noremap = true, silent = true })
vim.keymap.set("n", "j", "gj", { noremap = true, silent = true })
vim.keymap.set("n", "k", "gk", { noremap = true, silent = true })
vim.keymap.set("i", "<C-c>", "<ESC><ESC>", { noremap = true, silent = true })
vim.keymap.set("t", "<C-w>N", "<C-\\><C-n>", { noremap = true, silent = true })
vim.keymap.set("t", "<ESC>", "<C-\\><C-n>", { noremap = true, silent = true })
