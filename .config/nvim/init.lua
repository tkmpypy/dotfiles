if vim.loader then
  vim.loader.enable()
end

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

require("variables")
require("options")

require("plugins_lazy")

require("autocmds")
require("keymaps")
