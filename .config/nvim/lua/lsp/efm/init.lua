local M = {
  init_options = {
    documentFormatting = true,
    documentRangeFormatting = true,
    hover = true,
    documentSymbol = true,
    codeAction = true,
    completion = true,
  },
  settings = {
    rootMarkers = { ".git/" },
    languages = {},
  },
}

---return base path string
---@return string
local get_base_executable_path = function()
  return vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "bin")
end

---@param command string
---@return string
M.get_executable_path = function(command)
  return vim.fs.joinpath(get_base_executable_path(), command)
end

M.make_config = function()
  local stylua = require("lsp.efm.formatter.stylua")
  local gofumpt = require("lsp.efm.formatter.gofumpt")
  local goimports = require("lsp.efm.formatter.goimports")
  local golangci_lint = require("lsp.efm.linter.golangci_lint")
  local autopep8 = require("lsp.efm.formatter.autopep8")
  local flake8 = require("lsp.efm.linter.flake8")
  local isort = require("lsp.efm.formatter.isort")
  local tf_fmt = require("lsp.efm.formatter.terraform_fmt")
  local shfmt = require("lsp.efm.formatter.shfmt")
  local shellcheck = require("lsp.efm.linter.shellcheck")
  local sql_formatter = require("lsp.efm.formatter.sql_formatter")
  local prettier = require("lsp.efm.formatter.prettier")
  local eslint_d = require("lsp.efm.linter.eslint_d")
  local cspell = require("lsp.efm.linter.cspell")

  M.settings.languages = {
    lua = { stylua },
    go = { gofumpt, goimports, golangci_lint },
    python = { autopep8, isort, flake8 },
    terraform = { tf_fmt },
    sh = { shfmt, shellcheck },
    sql = { sql_formatter },
    javascript = { prettier, eslint_d },
    javascriptreact = { prettier, eslint_d },
    typescript = { prettier, eslint_d },
    typescriptreact = { prettier, eslint_d },
    vue = { prettier, eslint_d },
    css = { prettier },
    scss = { prettier },
    less = { prettier },
    html = { prettier },
    json = { prettier },
    jsonc = { prettier },
    yaml = { prettier },
    markdown = { prettier },
    ["="] = { cspell },
  }

  return {
    filetypes = vim.tbl_keys(M.settings.languages),
    init_options = M.init_options,
    settings = M.settings,
  }
end

return M
