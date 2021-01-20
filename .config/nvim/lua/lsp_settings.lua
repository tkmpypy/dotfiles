local prequire = function(...)
  local status, lib = pcall(require, ...)
  if (status) then return lib end
  -- Library failed to load, so perhaps return `nil` or something?
  return nil
end

local bin_dir = vim.fn.expand('~/.local/share/nvim/lsp')

local vim = vim
-- local lsp_status = require('lsp-status')
-- lsp_status.config({
--   indicator_errors = '✘',
--   indicator_warnings = '⚠',
--   indicator_info = 'כֿ',
--   indicator_hint = '•',
--   indicator_ok = '✓',
--   status_symbol = ' '
-- })
-- lsp_status.register_progress()

local completion = require('completion')
local nvim_lsp = require('lspconfig')
local lsp_configs = require('lspconfig/configs')

local custom_attach = function(client)
  if (client.config.flags) then
    client.config.flags.allow_incremental_sync = true
  end
  -- lsp_status.on_attach(client)
  completion.on_attach(client)
  -- vim.cmd("setlocal omnifunc=v:lua.vim.lsp.omnifunc")
end

local custom_capabilities = vim.lsp.protocol.make_client_capabilities()
custom_capabilities.textDocument.completion.completionItem.snippetSupport = true

vim.lsp.handlers["textDocument/publishDiagnostics"] =
    vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      -- Enable underline, use default values
      underline = true,
      -- Enable virtual text, override spacing to 4
      virtual_text = {spacing = 2, prefix = '»'},
      -- Use a function to dynamically turn signs off
      -- and on, using buffer local variables
      -- signs = function(bufnr, client_id)
      --   local ok, result = pcall(vim.api.nvim_buf_get_var, bufnr, 'show_signs')
      --   -- No buffer local variable set, so just enable by default
      --   if not ok then
      --     return true
      --   end
      -- 
      --   return result
      -- end,
      signs = {priority = 20},
      -- Disable a feature
      update_in_insert = false
    }
)

nvim_lsp.jsonls.setup({
  on_attach = custom_attach,
  capabilities = custom_capabilities
})

nvim_lsp.yamlls.setup({
  on_attach = custom_attach,
  capabilities = custom_capabilities
  -- settings = {
  --   yaml = {
  --     schemas = {
  --       ['http://json.schemastore.org/github-workflow'] = '.github/workflows/*.{yml,yaml}',
  --       ['http://json.schemastore.org/github-action'] = '.github/action.{yml,yaml}',
  --       kubernetes = "/*.y*ml",
  --     }
  --   }
  -- }
})

nvim_lsp.terraformls.setup({
  on_attach = custom_attach,
  capabilities = custom_capabilities
})

-- nvim_lsp.pyls_ms.setup({
--   init_options = {
--     interpreter = {
--       properties = {
--         InterpreterPath = '/Users/takuma/.pyenv/versions/3.5.2/bin/python',
--         Version = '3.5.2'
--       }
--     }
--   },
--   -- handlers = lsp_status.extensions.pyls_ms.setup(),
--   on_attach = custom_attach,
--   capabilities = custom_capabilities
-- })

nvim_lsp.pyright.setup({
  on_attach = custom_attach,
  capabilities = custom_capabilities,
})

nvim_lsp.dartls.setup({
  init_options = {
    closingLabels = true,
    flutterOutline = true,
    onlyAnalyzeProjectsWithOpenFiles = true,
    outline = true,
    suggestFromUnimportedLibraries = true
  },
  on_attach = custom_attach,
  capabilities = custom_capabilities,
  handlers = {
    ['dart/textDocument/publishClosingLabels'] = require(
        'lsp_extensions.dart.closing_labels').get_callback(
        {highlight = "Special", prefix = " >> "})
  }
})

nvim_lsp.vimls.setup({
  on_attach = custom_attach,
  capabilities = custom_capabilities
})

nvim_lsp.rust_analyzer.setup({
  on_attach = custom_attach,
  capabilities = custom_capabilities
})
-- nvim_lsp.rls.setup({
--   on_attach = custom_attach,
--   capabilities = lsp_status.capabilities
-- })

nvim_lsp.solargraph.setup({
  on_attach = custom_attach,
  capabilities = custom_capabilities
})

nvim_lsp.gopls.setup({
  on_attach = custom_attach,
  capabilities = custom_capabilities
})

local lua_lsp_dir = bin_dir .. '/lua/lua-language-server'
local lua_lsp_bin = lua_lsp_dir .. '/bin/macOS/lua-language-server'
nvim_lsp.sumneko_lua.setup{
  cmd = {lua_lsp_bin, '-E', lua_lsp_dir .. '/main.lua'},
  on_attach = custom_attach,
  capabilities = custom_capabilities,
  settings = {
    Lua = {
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
        },
      },
    },
  },
}


nvim_lsp.tsserver.setup({
  on_attach = custom_attach,
  capabilities = custom_capabilities
})
nvim_lsp.diagnosticls.setup {
  on_attach = custom_attach,
  capabilities = custom_capabilities,
  filetypes = {
    'javascript', 'javascriptreact', 'typescript', 'typescriptreact', 'go',
    'rust', 'lua'
  },
  init_options = {
    linters = {
      eslint = {
        command = './node_modules/.bin/eslint',
        rootPatterns = {'.git'},
        debounce = 500,
        args = {'--stdin', '--stdin-filename', '%filepath', '--format', 'json'},
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
        securities = {[2] = 'error', [1] = 'warning'}
      },
    },
    filetypes = {
      javascript = 'eslint',
      javascriptreact = 'eslint',
      typescript = 'eslint',
      typescriptreact = 'eslint',
      dart = 'dartanalyzer'
    }
    -- formatters = {
    --     dartfmt = {
    --         command = dart_sdk.."/dartfmt",
    --         args = {"--fix"},
    --         isStdout = true,
    --         isStderr = false,
    --     },
    --     prettier = {
    --         command = "./node_modules/.bin/prettier",
    --         args = {
    --             "--stdin-filepath", "%filepath", '--single-quote',
    --             '--print-width 120'
    --         }
    --     },
    --     eslint_fix = {
    --         command = "./node_modules/.bin/prettier",
    --         args = {
    --             "--fix", "--stdin-filepath", "%filepath", '--single-quote',
    --             '--print-width 120'
    --         }
    --     },
    --     rustfmt = {command = "rustfmt", args = {"%filepath"}},
    --     gofmt = {command = "gofmt", args = {"%filepath"}},
    --     goimports = {command = "goimports", args = {"%filepath"}},
    --     luaformatter = {command = "lua-format", args = {"-i"}}
    -- },
    -- formatFiletypes = {
    --     javascript = {"prettier", "eslint_fix"},
    --     javascriptreact = {"prettier", "eslint_fix"},
    --     typescript = {"prettier", "eslint_fix"},
    --     typescriptreact = {"prettier", "eslint_fix"},
    --     rust = "rustfmt",
    --     go = {"gofmt", "goimports"},
    --     lua = {"luaformatter"},
    --     dart = {"dartfmt"}
    -- }
  }
}

local lsp_util_codeaction = prequire('lsputil.codeAction')
if lsp_util_codeaction then
  vim.lsp.handlers['textDocument/codeAction'] =
      lsp_util_codeaction.code_action_handler
end
local lsp_util_locations = prequire('lsputil.locations')
if lsp_util_locations then
  vim.lsp.handlers['textDocument/references'] =
      lsp_util_locations.references_handler
  vim.lsp.handlers['textDocument/definition'] =
      lsp_util_locations.definition_handler
  vim.lsp.handlers['textDocument/declaration'] =
      lsp_util_locations.declaration_handler
  vim.lsp.handlers['textDocument/typeDefinition'] =
      lsp_util_locations.typeDefinition_handler
  vim.lsp.handlers['textDocument/implementation'] =
      lsp_util_locations.implementation_handler
end
local lsp_util_symbols = prequire('lsputil.symbols')
if lsp_util_symbols then
  vim.lsp.handlers['textDocument/documentSymbol'] =
      lsp_util_symbols.document_handler
  vim.lsp.handlers['workspace/symbol'] = lsp_util_symbols.workspace_handler
end

