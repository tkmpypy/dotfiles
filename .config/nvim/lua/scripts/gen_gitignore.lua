local vim = vim
local util = require("scripts/util")
local M = {}
local cache = { list = {} }

_G.tkmpypy = _G.tkmpypy or {}
_G.tkmpypy.Gigi = _G.tkmpypy.Gigi or {}

local regist_command = function()
  vim.cmd([[
    command! -nargs=1 -complete=customlist,v:lua.tkmpypy.Gigi.get_template_list Gigi call v:lua.tkmpypy.Gigi.generate_gitignore(<f-args>)
  ]])
end

local create_cmd = function(path)
  return 'curl -sL "https://www.toptal.com/developers/gitignore/api/' .. path .. '"'
end

function _G.tkmpypy.Gigi.get_template_list(arg, _, _)
  if vim.tbl_isempty(cache.list) then
    local cmd = create_cmd("list")
    local r = vim.fn.system(cmd)
    local lines = vim.split(r, "\n")
    for _, line in ipairs(lines) do
      local langs = vim.split(line, ",")
      vim.list_extend(cache.list, langs)
    end
  end

  local l = vim.tbl_filter(function(v)
    return util.str.starts_with(v, arg)
  end, cache.list)
  return l
end

function _G.tkmpypy.Gigi.generate_gitignore(args)
  if vim.api.nvim_get_option_value("modifiable", { buf = 0 }) then
    local cmd = create_cmd(args)
    local r = vim.fn.system(cmd)
    r = vim.split(r, "\n")
    vim.api.nvim_put(r, "l", false, true)
  end
end

M.initialize = function()
  regist_command()
end

return M
