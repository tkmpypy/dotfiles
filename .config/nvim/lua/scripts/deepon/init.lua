-- translator by Deepl on neovim

local M = {}
local fn = vim.fn

local dirname = "deepon"
local config_fname = "config.json"
local config_path = fn.stdpath("data") .. "/" .. dirname .. "/" .. config_fname

_G.tkmpypy = _G.tkmpypy or {}
_G.tkmpypy.Deepon = _G.tkmpypy.Deepon or {}

local default_opts = {
  lang = {
    primary = "ja",
    secondary = "en",
  },
  ui = {
    open = "split", -- vsplit, split or tab????
    editor = "vsplit" -- vsplit, split
  },
}

local regist_command = function()
  vim.cmd [[
    command! -nargs=0 DeeponEditToken call v:lua.tkmpypy.Deepon.edit_token()
    command! -nargs=0 DeeponTransP2S call v:lua.tkmpypy.Deepon.trans()
    command! -nargs=0 DeeponTransS2P call v:lua.tkmpypy.Deepon.trans_reverse()
  ]]
end

-- create config file to stdpath("data")
-- save API token. etc...
local initialize = function ()

  -- create config file to stdpath("data") when not found file

  -- check exists config file
  -- question api token and save to config file
end

-- primary to secondary
M.trans = function ()

end

-- secondary to primary
M.trans_revese = function ()

end

M.edit_token = function ()

end

M.setup = function (opts)

end

return M
