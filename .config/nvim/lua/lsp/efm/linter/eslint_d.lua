local efm = require("lsp.efm")

local bin = "eslint_d"
local command = string.format("%s --no-color --format visualstudio --stdin --stdin-filename ${INPUT}", efm.get_executable_path(bin))

local M = {
  prefix = bin,
  lintCommand = command,
  lintStdin = true,
  lintFormats = { "%f:%l:%c: %m" },
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
