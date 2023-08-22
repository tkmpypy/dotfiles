local efm = require("lsp.efm")

local bin = "golangci-lint"
local command =
  string.format("%s run --color never --out-format tab ${INPUT}", efm.get_executable_path(bin))

local M = {
  prefix = bin,
  lintCommand = command,
  lintStdin = false,
  lintFormats = { "%.%#:%l:%c %m" },
  rootMarkers = {},
}

return M
