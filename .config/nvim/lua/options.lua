local opt = vim.opt

opt.termguicolors = true
opt.shell = "fish"
opt.mouse = "n"
opt.laststatus = 3
opt.background = "dark"
opt.splitkeep = "screen"
opt.clipboard = { "unnamed", "unnamedplus" }
opt.encoding = "utf-8"
opt.fileencodings = "utf-8"
opt.re = 0
opt.exrc = true

opt.complete:remove({ "i", "t" })
opt.completeopt = { "menu", "menuone", "noselect" }
-- opt.ttyfast = true
opt.redrawtime = 300
opt.updatetime = 300
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
opt.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]]
opt.foldcolumn = "1" -- '0' is not bad
opt.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
opt.foldlevelstart = 99
opt.foldenable = true
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
opt.cmdheight = 0
opt.shortmess = "acTSFI"
opt.signcolumn = "yes:2"
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true
opt.showmatch = true
opt.swapfile = false
opt.autoread = true
opt.synmaxcol = 200
