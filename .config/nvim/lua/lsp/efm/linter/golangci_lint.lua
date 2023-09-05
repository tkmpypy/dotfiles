local efm = require("lsp.efm")

local bin = "golangci-lint"
local command = string.format("%s run --fast --color never --out-format line-number ./...", efm.get_executable_path(bin))

local M = {
  prefix = bin,
  lintCommand = command,
  lintStdin = false,
  lintFormats = {
    "%A%.%#:%l:%c: %m",
    "%C%.%#",
    "%Z%p^",
  },
  rootMarkers = {},
}

return M
