local efm = require("lsp.efm")

local bin = "eslint_d"
local command =
  string.format('%s --no-color --stdin-filename "${INPUT}" --stdin', efm.get_executable_path(bin))

local M = {
  prefix = bin,
  lintCommand = command,
  lintStdin = true,
  lintFormats = {
    "%-P%f",
    " %#%l:%c %# %trror  %m",
    " %#%l:%c %# %tarning  %m",
    "%-Q",
    "%-G%.%#",
  },
  lintIgnoreExitCode = true,
  rootMarkers = {
    ".eslintrc",
    ".eslintrc.cjs",
    ".eslintrc.js",
    ".eslintrc.json",
    ".eslintrc.yaml",
    ".eslintrc.yml",
    "package.json",
  },
}

return M
