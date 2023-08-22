local efm = require("lsp.efm")

local formatter = "isort"
local command = string.format("%s --stdout --filename ${INPUT} -", efm.get_executable_path(formatter))

local M = {
  formatCommand = command,
  formatStdin = true,
  rootMarkers = {
    ".isort.cfg",
    "pyproject.toml",
    "setup.py",
    "setup.cfg",
    "tox.ini",
    ".editorconfig",
  },
}

return M
