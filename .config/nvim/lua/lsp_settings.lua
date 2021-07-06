local prequire = function(...)
  local status, lib = pcall(require, ...)
  if (status) then return lib end
  -- Library failed to load, so perhaps return `nil` or something?
  return nil
end

local vim = vim

local custom_init = function(client)
  if (client.config.flags) then
    client.config.flags.allow_incremental_sync = true
  end
end

local custom_attach = function(client, bufnr)
  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec([[
    augroup lsp_document_highlight
    autocmd! * <buffer>
    autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
    autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
    augroup END
    ]], false)
  end
end

-- Configure lua language server for neovim development
local lua_config = {
  settings = {
    Lua = {
      runtime = {
        -- LuaJIT in the case of Neovim
        version = 'LuaJIT',
        path = vim.split(package.path, ';')
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'}
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true
        }
      }
    }
  }
}

local dart_config = {
  init_options = {
    closingLabels = true,
    flutterOutline = true,
    onlyAnalyzeProjectsWithOpenFiles = true,
    outline = true,
    suggestFromUnimportedLibraries = true
  }
}

local gopls_config = {
  init_options = {
    gofumpt = true,
    usePlaceholders = true,
    semanticTokens = true,
    staticcheck = true,
    experimentalPostfixCompletions = true,
    hoverKind = 'Structured',
    analyses = {
      nilness = true,
      shadow = true,
      unusedparams = true,
      unusedwrite = true,
      fieldalignment = true
    },
    codelenses = {gc_details = true, tidy = true}
  }
}

local diagnosticls_config = {
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
      golangci = {
        command = 'golangci-lint',
        rootPatterns = {'go.mod', '.golangci.yml', '.golangci.yaml'},
        debounce = 500,
        args = {'run', '--out-format', 'json', '--fast'},
        parseJson = {
          sourceName = "Pos.Filename",
          sourceNameFilter = true,
          errorsRoot = "Issues",
          line = "Pos.Line",
          column = "Pos.Column",
          message = "${Text} [${FromLinter}]"
        }
      }
    },
    filetypes = {
      javascript = 'eslint',
      javascriptreact = 'eslint',
      typescript = 'eslint',
      typescriptreact = 'eslint',
      dart = 'dartanalyzer',
      go = {'golangci'}
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

-- config that activates keymaps and enables snippet support
local make_config = function()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  return {
    -- enable snippet support
    capabilities = capabilities,
    -- map buffer local keybindings when the language server attaches
    on_attach = custom_attach,
    on_init = custom_init
  }
end

-- lsp-install
local function setup_servers()
  require'lspinstall'.setup()

  -- get all installed servers
  local servers = require'lspinstall'.installed_servers()
  -- ... and add manually installed servers
  -- table.insert(servers, "clangd")
  -- table.insert(servers, "sourcekit")
  table.insert(servers, "dartls")

  for _, server in pairs(servers) do
    local config = make_config()

    -- language specific config
    if server == "lua" then config.settings = lua_config.settings end

    if server == "dartls" then
      config.init_options = dart_config.init_options
      config.handlers = dart_config.handlers
    end

    if server == "gopls" then config.init_options = gopls_config.init_options end

    if server == "diagnosticls" then
      config.init_options = diagnosticls_config.init_options
      config.filetypes = diagnosticls_config.filetypes
    end

    require'lspconfig'[server].setup(config)
  end
end

setup_servers()

-- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
require'lspinstall'.post_install_hook = function()
  setup_servers() -- reload installed servers
  vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
end

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
      update_in_insert = true
    })

