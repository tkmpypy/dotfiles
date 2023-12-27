local formatter = "terraform"
local command = string.format(
  "%s fmt -",
  formatter
)

local M = {
    formatCommand = command,
    formatStdin = true,
  }

return M


