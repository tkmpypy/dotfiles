local opt = vim.opt

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
opt.scrolloff = 1
opt.sidescrolloff = 7

if opt.compatible then
  opt.compatible = false
end

opt.display:append("lastline")
opt.list = true
opt.listchars = {
  tab = "» ",
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
opt.signcolumn = "yes:2"
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true
opt.showmatch = true
opt.swapfile = false
opt.autoread = true
opt.synmaxcol = 200
