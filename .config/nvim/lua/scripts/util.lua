local api = vim.api
local M = {
  str = {},
  tbl = {},
  logger = {},
  keymaps = {},
  os = {},
  file = {},
  buffer = {},
  lib = {},
  lsp = {},
}

local function same_until(first, second)
  for i = 1, #first do
    if first[i] ~= second[i] then
      return i
    end
  end
  return 1
end

local function reverse(tbl)
  for i = 1, math.floor(#tbl / 2) do
    local j = #tbl - i + 1
    tbl[i], tbl[j] = tbl[j], tbl[i]
  end
end

M.tbl.set = function(list)
  local set = {}
  for _, l in ipairs(list) do
    set[l] = true
  end
  return set
end

M.tbl.insert_set = function(list, v)
  if not list[v] then
    table.insert(list, v)
    list[v] = true
  end
end

M.tbl.join = function(list, sep)
  local r = ""
  for _, v in pairs(list) do
    if r ~= "" then
      r = r .. sep .. v
    else
      r = v
    end
  end

  return r
end

M.buffer.scroll_bar_blocks = { "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█" }
M.buffer.get_vselected_value = function()
  vim.cmd([[
    let tmp=@@
    silent normal! gv""y
    let selected=@@
    let @@=tmp
  ]])

  local s = api.nvim_get_var("selected")
  return s
end
M.buffer.scroll_bar = function()
  local curr_line = api.nvim_win_get_cursor(0)[1]
  local lines = api.nvim_buf_line_count(0)

  local b = M.buffer.scroll_bar_blocks[math.floor(curr_line / lines * 7) + 1]
  if b == nil then
    return ""
  end
  return b .. b
end

M.file.get_tail = function(filename)
  return vim.fn.fnamemodify(filename, ":t")
end
M.file.split_filename = function(filename)
  local nodes = {}
  for parent in string.gmatch(filename, "[^/]+/") do
    table.insert(nodes, parent)
  end
  table.insert(nodes, M.file.get_tail(filename))
  return nodes
end
M.file.reverse_filename = function(filename)
  local parents = M.file.split_filename(filename)
  reverse(parents)
  return parents
end
M.file.get_unique_filename = function(filename, other_filenames)
  local rv = ""

  local others_reversed = vim.tbl_map(M.file.reverse_filename, other_filenames)
  local filename_reversed = M.file.reverse_filename(filename)
  local same_until_map = vim.tbl_map(function(second)
    return same_until(filename_reversed, second)
  end, others_reversed)

  local max = 0
  for _, v in ipairs(same_until_map) do
    if v > max then
      max = v
    end
  end
  for i = max, 1, -1 do
    rv = rv .. filename_reversed[i]
  end

  return rv
end

M.file.get_current_ufn = function()
  local buffers = vim.fn.getbufinfo()
  local listed = vim.tbl_filter(function(buffer)
    return buffer.listed == 1
  end, buffers)
  local names = vim.tbl_map(function(buffer)
    return buffer.name
  end, listed)
  local current_name = vim.fn.expand("%")
  return M.file.get_unique_filename(current_name, names)
end

M.os.icon = function()
  local os = vim.bo.fileformat:upper()
  local icon
  if os == "UNIX" then
    icon = " "
  elseif os == "MAC" then
    icon = " "
  else
    icon = " "
  end
  return icon .. os
end

M.str.starts_with = function(text, prefix)
  return text:find(prefix, 1, true) == 1
end
M.str.format = function(text, var_name, val)
  local tbl = {}
  tbl[var_name] = val
  return text:gsub("$(%w+)", tbl)
end
M.str.split = function(str, ts)
  if ts == nil then
    return {}
  end

  local t = {}
  local i = 1
  for s in string.gmatch(str, "([^" .. ts .. "]+)") do
    t[i] = s
    i = i + 1
  end

  return t
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
    if c == "<" then
      table.insert(result, c)
      ctrl = true
    elseif ctrl and c ~= ">" then
      table.insert(result, string.upper(c))
    elseif ctrl and c == ">" then
      table.insert(result, c)
      ctrl = false
    else
      table.insert(result, c)
    end
  end
  return table.concat(result, "")
end

M.keymaps.get_rhs = function(keymaps, mode, key)
  return vim.tbl_filter(function(v)
    local k = M.keymaps.to_upper(key)
    if v.mode == mode and v.lhs == k then
      return v.rhs
    end
    return false
  end, keymaps)
end

M.keymaps.default_opt = { silent = true, noremap = true }

M.lib.prequire = function(...)
  local status, lib = pcall(require, ...)
  if status then
    return lib
  end
  -- Library failed to load, so perhaps return `nil` or something?
  return nil
end

M.lsp.current_lsp = function()
  if vim.g.lsp_client_type == "neovim" then
    local msg = "No Active Lsp"
    local clients = vim.lsp.get_clients(nil)
    if next(clients) == nil then
      return msg
    end

    local client_names = M.tbl.set({})
    local null_ls_sources = M.tbl.set({})
    for _, client in ipairs(clients) do
      if client.name == "null-ls" then
        for _, source in ipairs(require("null-ls.sources").get_available(vim.bo.filetype)) do
          M.tbl.insert_set(null_ls_sources, source.name)
        end
        M.tbl.insert_set(client_names, "null-ls(" .. table.concat(null_ls_sources, ", ") .. ")")
      else
        M.tbl.insert_set(client_names, client.name)
      end
    end

    if #client_names >= 1 then
      msg = table.concat(client_names, "|")
    end
    return msg
  end

  return ""
end

M.lsp.toggle_diagnostics = function()
  local is_show = true
  return function()
    if is_show then
      is_show = false
      vim.diagnostic.hide()
    else
      is_show = true
      vim.diagnostic.show()
    end
  end
end

-- M.lsp.formatting = function(opts)
--   return vim.lsp.buf.format({
--     filter = function(client)
--       return client.name == "null-ls"
--     end,
--     async = opts.async,
--     bufnr = opts.bufnr,
--     range = opts.range,
--   })
-- end

M.nvim_create_augroups = function(definitions)
  for group_name, definition in pairs(definitions) do
    api.nvim_command("augroup " .. group_name)
    api.nvim_command("autocmd!")
    for _, def in ipairs(definition) do
      local command = table.concat(vim.iter({ "autocmd", def }):flatten():totable(), " ")
      api.nvim_command(command)
    end
    api.nvim_command("augroup END")
  end
end

return M
