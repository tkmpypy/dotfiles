--- inspired by https://github.com/alok/notational-fzf-vim

local vim = vim
local api = vim.api

local util = require("scripts/util")
local logger = util.logger
local M = {}

_G.tkmpypy = _G.tkmpypy or {}
_G.tkmpypy.Oboe = _G.tkmpypy.Oboe or {}

local _opts = {
	notes_dir = nil, -- set required
	find_cmd = nil, --set required. string or function(notes_dir)
	note = {
		extension = ".md",
		prefix = {
			enabled = true,
			date_format = "%Y-%m-%d",
		},
	},
}

local validation = function(opts)
	if opts.notes_dir == nil then
		logger.error("", "must set notes_dir config")
		return false
	end
	if opts.find_cmd == nil then
		logger.error("", "must set find_cmd config")
		return false
	end
	return true
end

local build_note_path = function(name)
	if _opts.note.prefix.enabled then
		local prefix = os.date(_opts.note.prefix_date_format)
		return vim.fn.expand(_opts.notes_dir .. "/" .. prefix .. "_" .. name .. _opts.note.extension)
	end

	return vim.fn.expand(_opts.notes_dir .. "/" .. name .. _opts.note.extension)
end

_G.tkmpypy.Oboe.create = function()
	local n = vim.fn.input("File name❯ ")
	if n == "" then
		return
	end

	local fname = build_note_path(n)
	logger.info("Oboe", fname)
	vim.cmd("e " .. fname)
end

_G.tkmpypy.Oboe.find = function()
	local cmd = nil
	if type(_opts.find_cmd) == "string" then
		cmd = _opts.find_cmd
	elseif type(_opts.find_cmd) == "function" then
		cmd = _opts.find_cmd(_opts.notes_dir)
	else
		logger.warn("Oboe", "not found command")
		return
	end

	api.nvim_command(cmd)
end

local regist_command = function()
	vim.cmd([[
    command! -nargs=0 OboeList  call v:lua.tkmpypy.Oboe.find()
    command! -nargs=0 OboeNew  call v:lua.tkmpypy.Oboe.create()
  ]])
end

M.setup = function(opts)
	if validation(opts) then
		_opts = vim.tbl_deep_extend("force", _opts, opts)
		regist_command()
	end
end

return M
