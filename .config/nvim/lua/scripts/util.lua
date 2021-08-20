local M = {str = {}, tbl = {}, logger = {}}

M.str.split = function(str, pattern, n)
  if not string.find(str, pattern) then return {str} end
  local result = {}
  local fpat = "(.-)" .. pattern
  local insertCnt = 1
  local lastEnd = 1
  local s, e, cap = string.find(str, fpat, 1)
  while s do
    if insertCnt > n and n > 0 then break end
    if s ~= 1 or cap ~= "" then table.insert(result, cap) end
    insertCnt = insertCnt + 1
    lastEnd = e + 1
    s, e, cap = string.find(str, fpat, lastEnd)
  end
  if lastEnd <= #str then
    cap = string.sub(str, lastEnd)
    table.insert(result, cap)
  end
  return result
end

M.str.starts_with =
    function(text, prefix) return text:find(prefix, 1, true) == 1 end

M.tbl.is_empty = function(tbl) return next(tbl) == nil end

M.logger.info = function(tag, text)
  vim.api.nvim_echo({{tag .. ' ' .. text, "InfoMsg"}}, true, {})
end

return M
