local vim = vim
local api = vim.api
local util = require("scripts/util")
local logger = util.logger
local M = {}

_G.tkmpypy = _G.tkmpypy or {}
_G.tkmpypy.GitLinker = _G.tkmpypy.GitLinker or {}

local get_head_commit_hash = function()
	return vim.fn.trim(vim.fn.system({ "git", "rev-parse", "HEAD" }))
end

local get_remote_url = function()
	local remote = vim.fn.trim(vim.fn.system({ "git", "remote", "show" }))
	return vim.fn.trim(vim.fn.system({ "git", "remote", "get-url", "--push", remote }))
end

local get_git_path = function(f)
	return vim.fn.trim(vim.fn.system({ "git", "ls-files", f }))
end

_G.tkmpypy.GitLinker.run = function()
	local h = get_head_commit_hash()
	if h == "" then
		logger.warn("GitLinker", "could not get commit hash.")
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
	local remote_url = get_remote_url()
  local link = remote_url .. '/' .. 'blob/' .. h .. '/' .. p

  print(link)
  local c = 'let @+ = "'..link..'"'
  vim.cmd(c)

end

-- TODO: dododo
local regist_command = function()
	vim.cmd([[
    command! -nargs=0 GitLinkerDefault call v:lua.tkmpypy.GitLinker.run()
    command! -nargs=0 GitLinkerCurrent call v:lua.tkmpypy.GitLinker.run()
  ]])
end

M.initialize = function()
	regist_command()
end

return M
