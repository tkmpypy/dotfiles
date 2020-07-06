local lsp_status = require('lsp-status')
local completion = require('completion')
-- local diagnostic = require('diagnostic')
local nvim_lsp = require('nvim_lsp')

local custom_attach = function(client)
    lsp_status.on_attach(client)
    completion.on_attach(client)
    -- diagnostic.on_attach(client)
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
  callbacks = lsp_status.extensions.pyls_ms.setup(),
  settings = { python = { workspaceSymbols = { enabled = true }}},
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

vim.g.indicator_errors = ''
vim.g.indicator_warnings = ''
vim.g.indicator_info = '🛈'
vim.g.indicator_hint = '!'
vim.g.indicator_ok = ''
vim.g.spinner_frames = {'⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷'}

-- for lsp_ext
-- vim.g.lsp_publish_diagnostics_severity_string_error = ''
-- vim.g.lsp_publish_diagnostics_severity_string_warning = ''
-- vim.g.lsp_publish_diagnostics_severity_string_info = '🛈'
-- vim.g.lsp_publish_diagnostics_severity_string_hint = '!'
