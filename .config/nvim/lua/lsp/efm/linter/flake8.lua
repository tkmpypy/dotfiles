local efm = require("lsp.efm")

local bin = "flake8"
local command = string.format(
  "%s --max-line-length 120 --stdin-display-name ${INPUT} -",
  efm.get_executable_path(bin)
)

local M = {
  prefix = bin,
  lintCommand = command,
  lintStdin = true,
  lintFormats = { "%f:%l:%c: %t%n %m" },
  rootMarkers = { "setup.cfg", "tox.ini", ".flake8" },
}

return M
