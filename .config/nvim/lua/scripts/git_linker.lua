local vim = vim
local api = vim.api
local util = require("scripts/util")
local logger = util.logger
local M = {}

local get_head_commit_hash = function()
  return vim.fn.trim(vim.fn.system({ "git", "rev-parse", "HEAD" }))
end

local get_default_branch_name = function()
  local ref = vim.fn.trim(vim.fn.system("git remote show origin | grep 'HEAD branch' | awk '{print $NF}'"))
  return ref
end

local get_remote_url = function(remote)
  local url = vim.fn.trim(vim.fn.system({ "git", "remote", "get-url", "--push", remote }))
  url = url:gsub("%.git", "")
  return url
end

local get_git_path = function(f)
  return vim.fn.trim(vim.fn.system({ "git", "ls-files", f }))
end

local yank = function(val)
  local c = 'let @+ = "' .. val .. '"'
  vim.cmd(c)
end

local create_line_val = function(s, e)
  -- #L1-L30
  return "#L" .. s .. "-L" .. e
end

local run = function(opts)
  local remote = vim.fn.trim(vim.fn.system({ "git", "remote", "show" }))

  local b = ""
  if opts.args == "current" then
    b = get_head_commit_hash()
    if b == "" then
      logger.warn("GitLinker", "could not get commit hash.")
      return
    end
  elseif opts.args == "default" then
    b = get_default_branch_name()
    if b == "" then
      logger.warn("GitLinker", "could not get branch name.")
      return
    end
  else
    logger.error("GitLinker", "current or default.")
    return
  end

  local f = api.nvim_buf_get_name(0)
  if f == "" then
    logger.warn("GitLinker", "could not get filename.")
    return
  end
  local p = get_git_path(f)
  if p == "" then
    logger.warn("GitLinker", "could not get filename under git.")
    return
  end
  local remote_url = get_remote_url(remote)
  local link = remote_url .. "/" .. "blob/" .. b .. "/" .. p

  if not (opts.line1 == 1 and opts.line2 == 1) then
    local line = create_line_val(opts.line1, opts.line2)
    link = link .. line
  end

  vim.notify_once(link, vim.log.levels.INFO)
  yank(link)
end

local create_command = function()
  vim.api.nvim_create_user_command("GitLinker", run, {
    bang = false,
    nargs = 1,
    range = true,
  })
end

M.setup = function()
  create_command()
end

return M
