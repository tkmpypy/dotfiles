local efm = require("lsp.efm")

local formatter = "gci"
local command = string.format(
  -- "%s write --skip-generated ${INPUT}",
  "%s print --skip-generated",
  efm.get_executable_path(formatter)
)

local M = {
    formatCommand = command,
    formatStdin = true,
    rootMarkers = { "go.mod", "go.sum" },
  }

return M
