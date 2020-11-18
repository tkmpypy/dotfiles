local prequire = function(...)
    local status, lib = pcall(require, ...)
    if (status) then return lib end
    -- Library failed to load, so perhaps return `nil` or something?
    return nil
end

local vim = vim
local lsp_status = require('lsp-status')
local completion = require('completion')
local nvim_lsp = require('lspconfig')

local custom_attach = function(client)
    lsp_status.on_attach(client)
    completion.on_attach(client)
    vim.cmd("setlocal omnifunc=v:lua.vim.lsp.omnifunc")
end

lsp_status.register_progress()

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    -- Enable underline, use default values
    underline = true,
    -- Enable virtual text, override spacing to 4
    virtual_text = {
      spacing = 2,
      prefix = '»',
    },
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
    signs = {
        priority = 20
    },
    -- Disable a feature
    update_in_insert = false,
  }
)

nvim_lsp.jsonls.setup({
    on_attach = custom_attach,
    capabilities = lsp_status.capabilities
})

nvim_lsp.yamlls.setup({
    on_attach = custom_attach,
    capabilities = lsp_status.capabilities
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
    cmd = {"terraform-ls", "serve"},
    on_attach = custom_attach,
    capabilities = lsp_status.capabilities
})

nvim_lsp.pyls_ms.setup({
    init_options = {
        interpreter = {
            properties = {
                InterpreterPath = '/Users/takuma/.pyenv/versions/3.5.2/bin/python',
                Version = '3.5.2'
            }
        }
    },
    handlers = lsp_status.extensions.pyls_ms.setup(),
    on_attach = custom_attach,
    capabilities = lsp_status.capabilities
})

local dart_sdk = vim.env.DART_SDK or ""
local dartls_cmd = {
    "dart", "./snapshots/analysis_server.dart.snapshot", "--lsp"
}
if dart_sdk ~= nil then
    dartls_cmd = {
        "dart", dart_sdk .. "/snapshots/analysis_server.dart.snapshot", "--lsp"
    }
end
nvim_lsp.dartls.setup({
    cmd = dartls_cmd,
    init_options = {
        closingLabels = "true",
        flutterOutline = "true",
        onlyAnalyzeProjectsWithOpenFiles = "false",
        outline = "true",
        suggestFromUnimportedLibraries = "true"
    },
    on_attach = custom_attach,
    capabilities = lsp_status.capabilities,
    handlers = {
        ['dart/textDocument/publishClosingLabels'] = require(
            'lsp_extensions.dart.closing_labels').get_callback(
            {highlight = "Special", prefix = " >> "})
    }
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
            "Color", "c", "Group", "g", "s"
        }
    })
end

nvim_lsp.tsserver.setup({
    on_attach = custom_attach,
    capabilities = lsp_status.capabilities
})
nvim_lsp.diagnosticls.setup {
    on_attach = custom_attach,
    capabilities = lsp_status.capabilities,
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
                args = {
                    '--stdin', '--stdin-filename', '%filepath', '--format',
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
                securities = {[2] = 'error', [1] = 'warning'}
            },
            dartanalyzer = {
                command = dart_sdk .. "/dartanalyzer",
                args = {'%filepath'},
                isStdout = true,
                isStderr = false,
                rootPatterns = {'.git', 'pubspec.yaml'},
                debounce = 500
            }
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

