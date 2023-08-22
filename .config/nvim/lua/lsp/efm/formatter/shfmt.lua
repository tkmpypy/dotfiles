local efm = require("lsp.efm")

local formatter = "shfmt"
local command = string.format(
  "%s -filename ${INPUT}",
  efm.get_executable_path(formatter)
)

local M = {
    formatCommand = command,
    formatStdin = true,
  }

return M


