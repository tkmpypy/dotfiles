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

local get_default_branch_name = function(remote)
	-- git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'
	local refs = vim.fn.trim(vim.fn.system("git symbolic-ref refs/remotes/" .. remote .. "/HEAD"))
	refs = util.str.split(refs, "/")
	if vim.tbl_isempty(refs) then
		return ""
	end
	return refs[#refs]
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

_G.tkmpypy.GitLinker.run = function(mode, start_line, end_line)
	local remote = vim.fn.trim(vim.fn.system({ "git", "remote", "show" }))

	local b = ""
	if mode == "current" then
		b = get_head_commit_hash()
		if b == "" then
			logger.warn("GitLinker", "could not get commit hash.")
			return
		end
	elseif mode == "default" then
		b = get_default_branch_name(remote)
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

	if not (start_line == 1 and end_line == 1) then
		local line = create_line_val(start_line, end_line)
		link = link .. line
	end

	print(link)
	yank(link)
end

local regist_command = function()
	vim.cmd([[
    command! -range -nargs=1 GitLinker call v:lua.tkmpypy.GitLinker.run(<f-args>,<line1>,<line2>)
  ]])
end

M.initialize = function()
	regist_command()
end

return M
