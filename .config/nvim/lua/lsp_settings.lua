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
    settings = {
        yaml = {
            schemas = {
                kubernetes = "/*.y*ml"
            }
        }
    }
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
nvim_lsp.gopls.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})
nvim_lsp.sumneko_lua.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})
nvim_lsp.tsserver.setup({
  on_attach = custom_attach,
  capabilities = lsp_status.capabilities
})

vim.lsp.callbacks['textDocument/codeAction'] = require'lsputil.codeAction'.code_action_handler
vim.lsp.callbacks['textDocument/references'] = require'lsputil.locations'.references_handler
vim.lsp.callbacks['textDocument/definition'] = require'lsputil.locations'.definition_handler
vim.lsp.callbacks['textDocument/declaration'] = require'lsputil.locations'.declaration_handler
vim.lsp.callbacks['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
vim.lsp.callbacks['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
vim.lsp.callbacks['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
vim.lsp.callbacks['workspace/symbol'] = require'lsputil.symbols'.workspace_handler

vim.g.indicator_errors = '✘'
vim.g.indicator_warnings = '⚠'
vim.g.indicator_info = 'כֿ'
vim.g.indicator_hint = '•'
vim.g.indicator_ok = '✔'
vim.g.spinner_frames = {'⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷'}

-- for lsp_ext
-- vim.g.lsp_publish_diagnostics_severity_string_error = ''
-- vim.g.lsp_publish_diagnostics_severity_string_warning = ''
-- vim.g.lsp_publish_diagnostics_severity_string_info = '🛈'
-- vim.g.lsp_publish_diagnostics_severity_string_hint = '!'
