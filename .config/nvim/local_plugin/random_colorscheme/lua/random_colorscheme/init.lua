local vim = vim
local M = {}

local set_random_colorscheme = function(colors_tbl)
  math.randomseed(os.clock() * 100000000000)
  local n = math.random(#colors_tbl)

  local cs = colors_tbl[n]
  vim.cmd("colorscheme " .. cs)
end

---setup function
---@param colors string[]
M.setup = function (colors)
  set_random_colorscheme(colors)
end

return M

