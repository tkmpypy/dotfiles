local vim = vim
local util = require('scripts/util')
local M = {}
local cache = {list = {}}

_G.gigi = _G.gigi or {}

local regist_command = function()
  vim.cmd [[
    command! -nargs=1 -complete=customlist,v:lua.gigi.get_template_list Gigi call v:lua.gigi.generate_gitignore(<f-args>)
  ]]
end

local create_cmd = function(path)
  return 'curl -sL "https://www.toptal.com/developers/gitignore/api/' .. path ..
             '"'
end

function _G.gigi.get_template_list(arg, _, _)
  if vim.tbl_isempty(cache.list) then
    local cmd = create_cmd('list')
    local r = vim.fn.system(cmd)
    r = r:gsub('\n', '')
    cache.list = util.str.split(r, ',', 0)
  end

  local l = vim.tbl_filter(function(v) return util.str.starts_with(v, arg) end,
                           cache.list)
  return l
end

function _G.gigi.generate_gitignore(args)
  local cmd = create_cmd(args)
  local r = vim.fn.system(cmd)
  r = util.str.split(r, '\n', 0)
  vim.api.nvim_put(r, "l", "", true)
end

M.setup = function() regist_command() end

return M
