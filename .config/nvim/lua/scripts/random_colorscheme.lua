local vim = vim
local util = require "scripts/util"
local logger = util.logger
local M = {}

M.set_random_colorscheme = function (colors_tbl)
  math.randomseed(os.time())
  local n = math.random(#colors_tbl)

  local cs = colors_tbl[n]
  logger.info("[random_colorscheme]", cs)
  vim.cmd('colorscheme '..cs)
end

return M
