local has_telescope, _ = pcall(require, 'telescope')
if not has_telescope then
  error('This plugins requires nvim-telescope/telescope.nvim')
end

local finders = require('telescope.finders')
local pickers = require('telescope.pickers')
local themes = require("telescope.themes")

local conf = require('telescope.config').values

local tkmpypy_telescope = {}
local default_opts = themes.get_dropdown{}

local _p = function (tbl)
  for i,v in ipairs(tbl) do
    local p = vim.fn.fnamemodify(v, ":.")
    tbl[i] = p
  end
  return tbl
end

tkmpypy_telescope.mru = function(opts)
  opts = opts or default_opts
  local t = vim.api.nvim_eval("mr#filter(mr#mru#list(), getcwd())")
  t = _p(t)
  pickers.new(opts, {
    prompt_title = "MRU",
    finder = finders.new_table {
      results = t
    },
    previewer = conf.qflist_previewer(opts),
    sorter = conf.generic_sorter(opts),
  }):find()
end

tkmpypy_telescope.mrw = function(opts)
  opts = opts or default_opts
  local t = vim.api.nvim_eval("mr#filter(mr#mrw#list(), getcwd())")
  t = _p(t)
  pickers.new(opts, {
    prompt_title = "MRW",
    finder = finders.new_table {
      results = t
    },
    previewer = conf.qflist_previewer(opts),
    sorter = conf.generic_sorter(opts),
  }):find()
end

return tkmpypy_telescope
