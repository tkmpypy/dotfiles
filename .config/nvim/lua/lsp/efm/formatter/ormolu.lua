local efm = require("lsp.efm")

local formatter = "ormolu"
local command = string.format(
  "%s ${INPUT}",
  efm.get_executable_path(formatter)
)

local M = {
    formatCommand = command,
    formatStdin = true,
    rootMarkers = { "stack.yaml", ".git" },
  }

return M

