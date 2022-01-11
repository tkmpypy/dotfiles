local vim = vim
local M = {}

M.set_random_coloescheme = function (colors_tbl)
  math.randomseed(os.time())
  local n = math.random(#colors_tbl)

  local cs = colors_tbl[n]
  print(cs)

  vim.cmd('colorscheme '..cs)
end

return M
