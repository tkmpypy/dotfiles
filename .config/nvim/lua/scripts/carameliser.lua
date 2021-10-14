local api = vim.api
local util = require("scripts/util")
local logger = util.logger
local M = {}

_G.tkmpypy = _G.tkmpypy or {}
_G.tkmpypy.Carameliser = _G.tkmpypy.Carameliser or {}

-- TODO: snake to camel
-- "hoge_fuga_piyo_foo_bar_baz" => "hogeFugaPiyoFooBarBaz"
-- ":%s/\v_(.)/\u\1/g"

-- TODO: snake to upper camel
-- "hoge_fuga_piyo_foo_bar_baz" => "HogeFugaPiyoFooBarBaz"
-- ":%s/\v(^|_)(.)/\u\2/g"

-- TODO: camel to snake
-- "hogeFugaPiyoFooBarBaz" => "hoge_fuga_piyo_foo_bar_baz"
-- "%s/\v([a-z]\@=)([A-Z])/\1_\l\2/g"

local validate = function(mode)
  if mode == "snake2camel" or mode == "snake2ucamel" or mode == "camel2snake" then
    return true
  end
  return false
end

-- mode: snake2camel, snake2ucamel, camel2snake
_G.tkmpypy.Carameliser.run = function(mode,start_line,end_line)
  if not validate(mode) then
    return
  end

  logger.info('s', start_line)
  logger.info('s', end_line)
end

local regist_command = function()
	vim.cmd([[
    command! -range -nargs=1 Carameliser call v:lua.tkmpypy.Carameliser.run(<f-args>,<line1>,<line2>)
  ]])
end

M.initialize = function()
	regist_command()
end

return M
