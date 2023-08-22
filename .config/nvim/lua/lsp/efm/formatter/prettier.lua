local efm = require("lsp.efm")

local formatter = "prettier"
local command = string.format(
  "%s --stdin --stdin-filepath ${INPUT} ${--range-start:charStart} "
    .. "${--range-end:charEnd} ${--tab-width:tabSize} ${--use-tabs:!insertSpaces}",
  efm.get_executable_path(formatter)
)

local M = {
  formatCommand = command,
  formatStdin = true,
  formatCanRange = true,
  rootMarkers = {
    ".prettierrc",
    ".prettierrc.json",
    ".prettierrc.js",
    ".prettierrc.yml",
    ".prettierrc.yaml",
    ".prettierrc.json5",
    ".prettierrc.mjs",
    ".prettierrc.cjs",
    ".prettierrc.toml",
    "prettier.config.js",
    "prettier.config.cjs",
    "package.json",
  },
}

return M
