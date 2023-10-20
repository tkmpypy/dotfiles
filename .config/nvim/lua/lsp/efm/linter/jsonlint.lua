local efm = require("lsp.efm")

local bin = "jsonlint"
local command =
  string.format("%s ${INPUT} --compact", efm.get_executable_path(bin))

local M = {
  prefix = bin,
  lintCommand = command,
  lintStdin = false,
  lintFormats = {
    "%f: line %l, col %c, %m",
  },
}

return M

