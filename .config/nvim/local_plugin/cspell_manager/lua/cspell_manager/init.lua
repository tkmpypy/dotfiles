local M = {}

---@alias CspellDictType "dotfiles"|"user"|

local data_dir = vim.fs.joinpath(vim.fn.stdpath("data"), "cspell")
local dotfiles_dict_path = vim.fs.joinpath(vim.fn.stdpath("config"), "cspell", "global.txt")
local local_dict_path = vim.fs.joinpath(data_dir, "user.txt")
local cspell_dic = {
  dotfiles = {
    name = "dotfiles",
    path = dotfiles_dict_path,
  },
  user = {
    name = "user",
    path = local_dict_path,
  },
}

---Insert word to dict
---@param dict_path string
---@param diagnostic Diagnostic
local insert_word = function(dict_path, diagnostic)
  local f, err = io.open(dict_path, "a+")
  if not f then
    vim.notify(err, vim.log.levels.ERROR, { title = "[cspell]" })
    return
  end

  local msg = diagnostic.message
  local w = msg:match("%b()")
  local word = w:sub(2, #w - 1)
  f:write(word, "\n")
  f:close()
  vim.notify(string.format("Added '%s'", word), vim.log.levels.INFO, { title = "[cspell]" })

  --`github>aquaproj/renovate-config`
  --If `aquaproj` is not registered in the dictionary and a spell check error occurs,
  --the string before `>` or `'` is also included in the number of `end_col`,
  --so the sum of `start_col` and the number of strings is used as `end_col`.
  --e.g.
  --The pattern should be `start_col=8` `end_col=15`, but in this pattern `start_col=8` `end_col=22`.

  -- replace word in buffer to trigger cspell to update diagnostics
  vim.api.nvim_buf_set_text(
    diagnostic.bufnr,
    diagnostic.lnum,
    diagnostic.col,
    diagnostic.end_lnum,
    diagnostic.col + #word,
    { word }
  )
end

---Add word to dict
---@param dict_type CspellDictType
local add_word = function(dict_type)
  local lnum = vim.api.nvim_win_get_cursor(0)[1] - 1
  local diagnostics = vim.diagnostic.get(0, { lnum = lnum })
  if vim.tbl_isempty(diagnostics) then
    return
  end

  for _, diagnostic in pairs(diagnostics) do
    if diagnostic.source == "cspell" then
      insert_word(cspell_dic[dict_type].path, diagnostic)
    end
  end
end

local create_command = function()
  vim.api.nvim_create_user_command("CspellAddWordManaged", function()
    add_word("dotfiles")
  end, { bang = false })
  vim.api.nvim_create_user_command("CspellAddWordLocal", function()
    add_word("user")
  end, { bang = false })
end

M.setup = function()
  -- vim辞書がなければダウンロード
  if vim.fn.filereadable(data_dir .. "/vim.txt.gz") ~= 1 then
    local vim_dictionary_url = "https://github.com/iamcco/coc-spell-checker/raw/master/dicts/vim/vim.txt.gz"
    io.popen("curl -fsSLo " .. data_dir .. "/vim.txt.gz --create-dirs " .. vim_dictionary_url)
  end

  -- ユーザー辞書がなければ作成
  if vim.fn.filereadable(cspell_dic.user.path) ~= 1 then
    io.popen("mkdir -p " .. data_dir)
    io.popen("touch " .. cspell_dic.user.path)
  end

  create_command()
end

return M


