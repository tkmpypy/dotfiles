local api = vim.api
local M = { str = {}, tbl = {}, logger = {}, keymaps = {} }

M.str.starts_with = function(text, prefix)
	return text:find(prefix, 1, true) == 1
end
M.str.format = function(text, var_name, val)
  local tbl = {}
  tbl[var_name] = val
  return text:gsub('$(%w+)', tbl)
end

M.logger.info = function(tag, text)
	api.nvim_echo({ { tag .. " " .. text, "InfoMsg" } }, true, {})
end
M.logger.error = function(tag, text)
	api.nvim_echo({ { tag .. " " .. text, "ErrorMsg" } }, true, {})
end
M.logger.warn = function(tag, text)
	api.nvim_echo({ { tag .. " " .. text, "WarningMsg" } }, true, {})
end
M.logger.inspect = function(text)
	print(vim.inspect(text))
end

M.keymaps.to_upper = function(keys)
  local result = {}
  local ctrl = false
  for i = 1, #keys do
    local c = string.sub(keys, i, i)
    if c == '<' then
      table.insert(result, c)
      ctrl = true
    elseif ctrl and c ~= '>' then
      table.insert(result, string.upper(c))
    elseif ctrl and c == '>' then
      table.insert(result, c)
      ctrl = false
    else
      table.insert(result, c)
    end
  end
  return table.concat(result, '')
end

M.keymaps.get_rhs = function(keymaps, mode, key)
	return vim.tbl_filter(function(v)
    local k = M.keymaps.to_upper(key)
		if v.mode == mode and v.lhs == k then
			return v.rhs
		end
		return nil
	end, keymaps)
end

return M
