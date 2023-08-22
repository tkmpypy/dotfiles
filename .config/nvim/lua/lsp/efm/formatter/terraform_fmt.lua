local efm = require("lsp.efm")

local formatter = "terraform"
local command = string.format(
  "%s fmt -",
  efm.get_executable_path(formatter)
)

local M = {
    formatCommand = command,
    formatStdin = true,
  }

return M


