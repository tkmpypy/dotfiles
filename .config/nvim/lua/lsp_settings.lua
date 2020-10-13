local prequire = function(...)
    local status, lib = pcall(require, ...)
    if(status) then return lib end
    --Library failed to load, so perhaps return `nil` or something?
    return nil
end

local vim = vim
local lsp_status = require('lsp-status')
local completion = require('completion')
local diagnostic = require('diagnostic')
local nvim_lsp = require('nvim_lsp')

local custom_attach = function(client)
    lsp_status.on_attach(client)
    completion.on_attach(client)
    diagnostic.on_attach(client)
end

lsp_status.register_progress()

nvim_lsp.jsonls.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})

nvim_lsp.yamlls.setup({
    on_attach = custom_attach,
    capabilities = lsp_status.capabilities,
})

nvim_lsp.terraformls.setup({
    cmd = {"terraform-ls", "serve"},
    on_attach = custom_attach,
    capabilities = lsp_status.capabilities,
})

nvim_lsp.pyls_ms.setup({
  init_options = {
    interpreter = {
      properties = {
        InterpreterPath = '/Users/takuma/.pyenv/versions/3.5.2/bin/python',
        Version = '3.5.2',
      }
    }
  },
  callbacks = lsp_status.extensions.pyls_ms.setup(),
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})

nvim_lsp.vimls.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})

nvim_lsp.rust_analyzer.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})

nvim_lsp.solargraph.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})

nvim_lsp.gopls.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})

nvim_lsp.sumneko_lua.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})
local nlua_lsp_nvim = prequire('nlua.lsp.nvim')
if nlua_lsp_nvim then
  nlua_lsp_nvim.setup(nvim_lsp, {
    on_attach = custom_attach,

    -- Include globals you want to tell the LSP are real :)
    globals = {
      -- Colorbuddy
      "Color", "c", "Group", "g", "s",
    }
  })
end

nvim_lsp.tsserver.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})
nvim_lsp.diagnosticls.setup{
  filetypes={'javascript', 'javascriptreact', 'typescript', 'typescriptreact'},
  init_options = {
    linters = {
      eslint = {
        command = './node_modules/.bin/eslint',
        rootPatterns = {'.git'},
        debounce = 100,
        args = {
          '--stdin',
          '--stdin-filename',
          '%filepath',
          '--format',
          'json'
        },
        sourceName = 'eslint',
        parseJson = {
          errorsRoot = '[0].messages',
          line = 'line',
          column = 'column',
          endLine = 'endLine',
          endColumn = 'endColumn',
          message = '${message} [${ruleId}]',
          security = 'severity'
        },
        securities = {
          [2] = 'error',
          [1] = 'warning',
        },
      },
    },
    filetypes = {
      javascript = 'eslint',
      javascriptreact = 'eslint',
      typescript = 'eslint',
      typescriptreact = 'eslint'
    },
    formatters = {
      prettier = {
        command = "./node_modules/.bin/prettier",
        args = {"--stdin-filepath" ,"%filepath", '--single-quote', '--print-width 120'}
      },
      eslint_fix = {
        command = "./node_modules/.bin/prettier",
        args = {"--fix", "--stdin-filepath" ,"%filepath", '--single-quote', '--print-width 120'}
      },
      rustfmt = {
        command = "rustfmt",
        args = {"%filepath"}
      },
      gofmt = {
        command = "gofmt",
        args = {"%filepath"}
      },
      goimports = {
        command = "goimports",
        args = {"%filepath"}
      },
    },
    formatFiletypes = {
      javascript = {"prettier", "eslint_fix"},
      javascriptreact = {"prettier", "eslint_fix"},
      typescript = {"prettier", "eslint_fix"},
      typescriptreact = {"prettier", "eslint_fix"},
      rust = "rustfmt",
      go = {"gofmt", "goimports"}
    },
  }
}

local lsp_util_codeaction = prequire('lsputil.codeAction')
if lsp_util_codeaction then
  vim.lsp.callbacks['textDocument/codeAction'] = lsp_util_codeaction.code_action_handler
end
local lsp_util_locations = prequire('lsputil.locations')
if lsp_util_locations then
  vim.lsp.callbacks['textDocument/references'] = lsp_util_locations.references_handler
  vim.lsp.callbacks['textDocument/definition'] = lsp_util_locations.definition_handler
  vim.lsp.callbacks['textDocument/declaration'] = lsp_util_locations.declaration_handler
  vim.lsp.callbacks['textDocument/typeDefinition'] = lsp_util_locations.typeDefinition_handler
  vim.lsp.callbacks['textDocument/implementation'] = lsp_util_locations.implementation_handler
end
local lsp_util_symbols = prequire('lsputil.symbols')
if lsp_util_symbols then
  vim.lsp.callbacks['textDocument/documentSymbol'] = lsp_util_symbols.document_handler
  vim.lsp.callbacks['workspace/symbol'] = lsp_util_symbols.workspace_handler
end

vim.g.indicator_errors = '✘'
vim.g.indicator_warnings = '⚠'
vim.g.indicator_info = 'כֿ'
vim.g.indicator_hint = '•'
vim.g.indicator_ok = '✔'
vim.g.spinner_frames = {'⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷'}

