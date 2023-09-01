local efm = require("lsp.efm")

local formatter = "autopep8"
local command = string.format(
  "%s -",
  efm.get_executable_path(formatter)
)

local M = {
    formatCommand = command,
    formatStdin = true,
  }

return M

