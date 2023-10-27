---ref: https://github.com/reviewdog/errorformat/tree/master/fmts
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
  -- single_file_support = false
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
  local linters = require("lsp.efm.linter")
  local formatters = require("lsp.efm.formatter")
  local stylua = formatters.stylua
  local gofumpt = formatters.gofumpt
  local goimports = formatters.goimports
  local gci = formatters.gci
  local golangci_lint = linters.golangci_lint
  local autopep8 = formatters.autopep8
  local flake8 = linters.flake8
  local isort = formatters.isort
  local tf_fmt = formatters.terraform_fmt
  local shfmt = formatters.shfmt
  local shellcheck = linters.shellcheck
  local sql_formatter = formatters.sql_formatter
  local prettier = formatters.prettier
  local eslint_d = linters.eslint_d
  local cspell = linters.cspell
  local jsonlint = linters.jsonlint

  M.settings.languages = {
    lua = { stylua },
    go = { golangci_lint,  goimports, gci, gofumpt },
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
    json = { prettier, jsonlint },
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
