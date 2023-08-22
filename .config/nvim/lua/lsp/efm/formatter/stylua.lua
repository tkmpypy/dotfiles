local efm = require("lsp.efm")

local formatter = "stylua"
local command = string.format(
  "%s ${--indent-width:tabSize} ${--range-start:charStart} " .. "${--range-end:charEnd} --color Never -",
  efm.get_executable_path(formatter)
)

local M = {
    formatCanRange = true,
    formatCommand = command,
    formatStdin = true,
    rootMarkers = { "stylua.toml", ".stylua.toml" },
  }

return M
