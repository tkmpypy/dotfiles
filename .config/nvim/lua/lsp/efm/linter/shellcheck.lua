local efm = require("lsp.efm")

local bin = "shellcheck"
local command = string.format("%s --color=never --format=gcc -", efm.get_executable_path(bin))

local M = {
  prefix = bin,
  lintCommand = command,
  lintStdin = true,
  lintFormats = {
    "-:%l:%c: %trror: %m",
    "-:%l:%c: %tarning: %m",
    "-:%l:%c: %tote: %m",
  },
  rootMarkers = {},
}

return M
