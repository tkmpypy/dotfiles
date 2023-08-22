local efm = require("lsp.efm")

local formatter = "rustfmt"
local command = string.format("%s --emit=stdout", efm.get_executable_path(formatter))

-- if plenary is available, we can try to read the Rust edition from Cargo.toml
-- source: https://github.com/jose-elias-alvarez/null-ls.nvim/wiki/Source-specific-Configuration#rustfmt
local ok, Path = pcall(require, "plenary.path")
if ok then
  local util = require("lspconfig.util")
  local root = util.root_pattern("Cargo.toml")(vim.loop.cwd())
  if root then
    local cargo_toml = Path:new(root .. "/" .. "Cargo.toml")
    if cargo_toml:exists() and cargo_toml:is_file() then
      for _, line in ipairs(cargo_toml:readlines()) do
        local edition = line:match([[^edition%s*=%s*%"(%d+)%"]])
        if edition then
          command = string.format("%s --edition=%s", command, edition)
        end
      end
    end
  end
end

local M = {
  formatCommand = command,
  formatStdin = true,
  rootMarkers = { "Cargo.toml" },
}

return M
