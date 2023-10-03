local efm = require("lsp.efm")
local M = {}

local bin = "golangci-lint"
local command = string.format("%s run --fast --color never --out-format line-number ./...", efm.get_executable_path(bin))

M = {
  prefix = bin,
  lintCommand = command,
  lintStdin = true,
  lintFormats = {
    "%E%f:%l:%c: %m",
    "%E%f:%l: %m",
    "%C%.%#"
  },
  rootMarkers = {"go.mod", ".git"},
}

return M
