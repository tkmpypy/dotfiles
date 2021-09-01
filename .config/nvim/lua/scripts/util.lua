local M = { str = {}, tbl = {}, logger = {} }

M.str.starts_with = function(text, prefix)
	return text:find(prefix, 1, true) == 1
end

M.logger.info = function(tag, text)
	vim.api.nvim_echo({ { tag .. " " .. text, "InfoMsg" } }, true, {})
end

return M
