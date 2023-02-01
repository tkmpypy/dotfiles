local vim = vim
local M = {}

M.set_random_colorscheme = function(colors_tbl)
  math.randomseed(os.clock() * 100000000000)
  local n = math.random(#colors_tbl)

  local cs = colors_tbl[n]
  vim.cmd("colorscheme " .. cs)
  vim.notify_once(cs, vim.log.levels.INFO, {
    title = "colorscheme",
  })
end

return M
