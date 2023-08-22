local efm = require("lsp.efm")

local formatter = "goimports"
local command = string.format(
  "%s",
  efm.get_executable_path(formatter)
)

local M = {
    formatCommand = command,
    formatStdin = true,
    rootMarkers = { "go.mod", "go.sum" },
  }

return M
