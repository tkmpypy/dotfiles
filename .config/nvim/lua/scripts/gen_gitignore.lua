local vim = vim
local util = require("scripts/util")
local M = {}
local cache = { list = {} }

local create_request = function(path)
  return string.format('curl -sL "https://www.toptal.com/developers/gitignore/api/%s"', path)
end

local get_template_list = function(arg, _, _)
  if vim.tbl_isempty(cache.list) then
    local cmd = create_request("list")
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

local generate_gitignore = function(opts)
  if vim.api.nvim_get_option_value("modifiable", { buf = 0 }) then
    local cmd = create_request(opts.args)
    local res = vim.fn.system(cmd)
    local r = vim.split(res, "\n")
    vim.api.nvim_put(r, "l", false, false)
  end
end

local create_command = function()
  vim.api.nvim_create_user_command("Gigi", generate_gitignore, {
    bang = false,
    nargs = 1,
    complete = get_template_list,
  })
end

M.setup = function()
  create_command()
end

return M
