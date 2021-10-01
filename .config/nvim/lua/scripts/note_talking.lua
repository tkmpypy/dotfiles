--- inspired by https://github.com/alok/notational-fzf-vim

local vim = vim
local api = vim.api
local uv = vim.loop

local util = require("scripts/util")
local logger = util.logger
local M = {}

_G.tkmpypy = _G.tkmpypy or {}
_G.tkmpypy.Oboe = _G.tkmpypy.Oboe or {}

local _opts = {
	base_dir = nil, -- set required
	func_find = nil, -- set required
	input = {
		prefix_len = nil, --set required
		offset = 0,
	},
	file_extension = ".md",
	keymap = {
		create_note = {
			mode = "t",
			map = "<C-x>",
		},
		delete_note = {
			mode = "t",
			map = "<C-d>",
		},
	},
}

local context = {
	bufnr = nil,
	winnr = nil,
	prev_c_map = nil,
	prev_d_map = nil,
}

local validation = function(opts)
	if opts.base_dir == nil then
		logger.error("", "must set base_dir config")
		return false
	end
	if opts.func_find == nil then
		logger.error("", "must set func_find config")
		return false
	end
	if opts.input.prefix_len == nil then
		logger.error("", "must set input.prefix_len config")
		return false
	end
	return true
end
-- {
--     buffer = 0,
--     expr = 0,
--     lhs = "<Plug>(vsnip-jump-prev)",
--     lnum = 130,
--     mode = "i",
--     noremap = 1,
--     nowait = 0,
--     rhs = "<Esc>:<C-U>call <SNR>124_jump(-1)<CR>",
--     script = 0,
--     sid = 124,
--     silent = 1
--   }
local restore_keymap = function(bufnr, keymap)
	api.nvim_buf_set_keymap(bufnr, keymap.mode, keymap.lhs, keymap.rhs, {
		nowait = keymap.nowait,
		silent = keymap.silent,
		noremap = keymap.noremap,
		script = keymap.script,
		sid = keymap.sid,
	})
end

local get_keymap_buf = function(bufnr, mode, lhs)
	local prev_maps = api.nvim_buf_get_keymap(bufnr, mode)
	return util.keymaps.get_rhs(prev_maps, mode, lhs)
end

local create_note_name = function(name)
  return _opts.base_dir .. '/' .. name .. _opts.file_extension
end

_G.tkmpypy.Oboe.create_note = function()
	local lineval = api.nvim_get_current_line()
	local current_col = api.nvim_win_get_cursor(0)[2]

	local start_col = _opts.input.prefix_len + _opts.input.offset
	current_col = current_col + _opts.input.offset
	lineval = lineval:sub(start_col+1, current_col)
	logger.info("", lineval)

  -- close fuzzy finder
  api.nvim_buf_delete(context.bufnr, {force = true})
  api.nvim_win_close(context.winnr, {force = true})

  local new_note = create_note_name(lineval)
  api.nvim_command('e '..new_note)
end

_G.tkmpypy.Oboe.delete_note = function()
	logger.info("", "delete note")
end

_G.tkmpypy.Oboe.restore_or_delete_keymap = function()
	if not vim.tbl_isempty(context.prev_c_map) then
		restore_keymap(context.bufnr, context.prev_c_map)
	else
		api.nvim_buf_del_keymap(context.bufnr, _opts.keymap.create_note.mode, _opts.keymap.create_note.map)
	end
	if not vim.tbl_isempty(context.prev_d_map) then
		restore_keymap(context.bufnr, context.prev_d_map)
	else
		api.nvim_buf_del_keymap(context.bufnr, _opts.keymap.delete_note.mode, _opts.keymap.delete_note.map)
	end
end

_G.tkmpypy.Oboe.on_term_enter = function()
	logger.info("", "on_term_enter")
  context.winnr = api.nvim_get_current_win()
	context.input.start_col = api.nvim_win_get_cursor(context.winnr)[2]
end

_G.tkmpypy.Oboe.run = function()
	_opts.func_find(_opts.base_dir)

	-- regist autocmd for buffer-local
	-- e.g. `:au CursorHold <buffer=33>  echo 'hold'`
	local timer = uv.new_timer()
	timer:start(
		500,
		0,
		vim.schedule_wrap(function()
			context.bufnr = api.nvim_get_current_buf()
			context.prev_c_map = get_keymap_buf(
				context.bufnr,
				_opts.keymap.create_note.mode,
				_opts.keymap.create_note.map
			)
			context.prev_d_map = get_keymap_buf(
				context.bufnr,
				_opts.keymap.delete_note.mode,
				_opts.keymap.delete_note.map
			)
			local au = util.str.format(
				[[
      augroup tkmpypy_oboe
        autocmd BufDelete,BufLeave <buffer=$bufnr> ++once call v:lua.tkmpypy.Oboe.restore_or_delete_keymap()
      augroup end
    ]],
				"bufnr",
				context.bufnr
			)
			vim.cmd(au)

			api.nvim_buf_set_keymap(
				context.bufnr,
				_opts.keymap.create_note.mode,
				_opts.keymap.create_note.map,
				"<cmd>:lua tkmpypy.Oboe.create_note()<CR>",
				{ nowait = true, silent = true, noremap = false }
			)
			api.nvim_buf_set_keymap(
				context.bufnr,
				_opts.keymap.delete_note.mode,
				_opts.keymap.delete_note.map,
				"<cmd>:lua tkmpypy.Oboe.delete_note()<CR>",
				{ nowait = true, silent = true, noremap = false }
			)
		end)
	)
end

local regist_command = function()
	vim.cmd([[
    command! -nargs=0 Oboe  call v:lua.tkmpypy.Oboe.run()
  ]])
end

local init_au = function()
	vim.cmd([[
    augroup tkmpypy_oboe
      autocmd!
    augroup end
  ]])
end

M.setup = function(opts)
	if validation(opts) then
		_opts = vim.tbl_deep_extend("force", _opts, opts)
		init_au()
		regist_command()
	end
end

return M
