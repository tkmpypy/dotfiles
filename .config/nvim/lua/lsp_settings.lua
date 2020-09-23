local prequire = function(...)
    local status, lib = pcall(require, ...)
    if(status) then return lib end
    --Library failed to load, so perhaps return `nil` or something?
    return nil
end

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

-- for lsp_ext
-- vim.g.lsp_publish_diagnostics_severity_string_error = ''
-- vim.g.lsp_publish_diagnostics_severity_string_warning = ''
-- vim.g.lsp_publish_diagnostics_severity_string_info = '🛈'
-- vim.g.lsp_publish_diagnostics_severity_string_hint = '!'
