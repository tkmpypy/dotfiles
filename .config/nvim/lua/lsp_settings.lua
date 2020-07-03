local lsp_status = require('lsp-status')
local completion = require('completion')
local diagnostic = require('diagnostic')
local nvim_lsp = require('nvim_lsp')

local custom_attach = function(client)
    lsp_status.on_attach(client)
    completion.on_attach(client)
    diagnostic.on_attach(client)
end

local function isModuleAvailable(name)
  if package.loaded[name] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
      local loader = searcher(name)
      if type(loader) == 'function' then
        package.preload[name] = loader
        return true
      end
    end
    return false
  end
end

if isModuleAvailable('nvim-treesitter.configs') then
    require'nvim-treesitter.configs'.setup {
        highlight = {
            enable = true,
        },
        incremental_selection = {
            enable = true,
        },
        refactor = {
          highlight_defintions = {
            enable = true
          },
          smart_rename = {
            enable = false,
          },
          navigation = {
            enable = false,
          }
        },
        ensure_installed = 'all'
    }
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
