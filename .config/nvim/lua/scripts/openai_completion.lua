local M = {}
local base_url = "https://api.openai.com/v1"
local end_of_text = "<|endoftext|>"

---@class OpenaiCompletion.Config
---@field model string
---@field max_tokens? integer
---@field prefix? fun():string
---@field suffix? fun():string
---@field presence_penalty integer
---@field frequency_penalty integer
---@field top_p integer
---@field n integer

---@class OpenaiCompletion.CompletionRequest
---@field model string
---@field prompt string
---@field temperature integer
---@field max_tokens integer
---@field stream boolean
---@field presence_penalty integer
---@field frequency_penalty integer
---@field top_p integer
---@field n integer

---@class OpenaiCompletion.CompletionResponseChoises
---@field text string
---@field index integer
---@field logprobs integer
---@field finish_reason string

---@class OpenaiCompletion.CompletionResponse
---@field id string
---@field object string
---@field created integer
---@field choices OpenaiCompletion.CompletionResponseChoises[]
---@field model string

---@class OpenaiCompletion.Context
local _context = {}

---@type OpenaiCompletion.Config
local default_config = {
	model = "text-davinci-003",
	max_tokens = 300,
	prefix = function()
		local ft = vim.api.nvim_buf_get_option(0, "filetype")
		return string.format("Please reply with source code only. use language is %s%s", ft, end_of_text)
	end,
	suffix = nil,
	presence_penalty = 0,
	frequency_penalty = 0,
	top_p = 1,
	n = 1,
}

---create command string. bellow:
---  curl -v https://api.openai.com/v1/completions -H 'Content-Type: application/json' -H 'Authorization: Bearer '${OPENAI_API_KEY} -d '{
---   "model": "text-davinci-003",
---   "stream": true,
---   "prompt": "Please reply only with source code<|endoftext|>fizzbuzzする関数を教えて<|endoftext|>",
---   "temperature": 0,
---   "max_tokens": 300,
---   "presence_penalty": 0,
---   "frequency_penalty": 0,
---   "top_p": 1,
---   "n": 1
--- }'
---@param cmd string
---@param args string[]
---@return string
local create_cmd = function(cmd, args)
	local c = cmd
	local a = table.concat(args, " ")
	return c .. " " .. a
end

---request completions api
---@param params OpenaiCompletion.CompletionRequest
---@param on_success fun(resp: table)
---@param on_error fun(err: table)
---@param on_exit function()
local request_completions = function(params, on_success, on_error, on_exit)
	local encoded = vim.fn.json_encode(params)
	local cmd = create_cmd("curl", {
		"-sS",
		"--no-buffer",
		base_url .. "/completions",
		"-H",
		"'Content-Type: application/json'",
		"-H",
		"'Authorization: Bearer " .. vim.env.OPENAI_API_KEY .. "'",
		"-d",
		"'" .. encoded .. "'",
	})

	vim.fn.jobstart(cmd, {
		on_stdout = function(_, d, _)
			on_success(d)
		end,
		on_stderr = function(_, d, _)
			if #d > 0 and #d[1] > 0 then
				on_error(d)
			end
		end,
		on_exit = function()
			on_exit()
		end,
	})
end

---json from string
---@param s string
---@return table?
local json_from_stream = function(s)
	local v = string.gsub(s, "data: ", "")
	return vim.json.decode(v)
end

local on_stream_error = function(err)
	vim.notify(vim.inspect(err), vim.log.levels.ERROR, { title = "Openai Completion" })
end

---check stream end
---@param s string
---@return boolean
local is_stream_end = function(s)
	if string.find(s, "data: %[DONE%]") then
		return true
	end
	return false
end

local on_stream_end = function()
	vim.notify("complete", vim.log.levels.INFO, { title = "Openai Completion" })
end

M.complete_code = function()
	local prompt = ""
	vim.ui.input({ prompt = "Input: " }, function(input)
		if input == nil or #input == 0 then
			return
		end

		local bufnr = vim.api.nvim_get_current_buf()
		local winnr = vim.api.nvim_get_current_win()

		prompt = _context.config.prefix() .. input .. end_of_text
		local suffix = nil
		if _context.config.suffix ~= nil then
			suffix = _context.config.suffix()
		end

		vim.api.nvim_command("startinsert!")

		request_completions({
			model = _context.config.model,
			prompt = prompt,
			temperature = 0,
			stream = true,
			suffix = suffix,
			presence_penalty = _context.config.presence_penalty,
			frequency_penalty = _context.config.frequency_penalty,
			top_p = _context.config.top_p,
			n = _context.config.n,
			max_tokens = _context.config.max_tokens,
		}, function(resp)
			if #resp <= 0 then
				return
			end

			for _, r in ipairs(resp) do
				local s = vim.fn.trim(r)
				if not is_stream_end(s) and #s > 0 then
					---@type OpenaiCompletion.CompletionResponse?
					local json = json_from_stream(s)
					if json ~= nil then
						for _, v in pairs(json.choices) do
							local row, col = unpack(vim.api.nvim_win_get_cursor(winnr))
							local lines = vim.split(v.text, "\n")
							vim.api.nvim_buf_set_text(bufnr, row - 1, col, row - 1, col, lines)
							vim.api.nvim_command("normal! $")
							row, col = unpack(vim.api.nvim_win_get_cursor(winnr))
							vim.api.nvim_win_set_cursor(winnr, { row, col + 1 })
						end
					end
				end
			end
		end, on_stream_error, on_stream_end)
	end)
end

M.setup = function(opts)
	_context.config = default_config
	if opts then
		_context.config = vim.tbl_deep_extend("force", default_config, opts)
	end
end

return M
