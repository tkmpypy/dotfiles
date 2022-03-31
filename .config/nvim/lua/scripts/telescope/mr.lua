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

tkmpypy_telescope.mru = function(opts)
  opts = opts or default_opts
  local t = vim.api.nvim_eval("mr#filter(mr#mru#list(), getcwd())")
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
