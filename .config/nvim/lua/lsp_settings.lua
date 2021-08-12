local prequire = function(...)
  local status, lib = pcall(require, ...)
  if (status) then return lib end
  -- Library failed to load, so perhaps return `nil` or something?
  return nil
end

local vim = vim

local custom_init = function(client)
  if (client.config.flags) then
    client.config.flags.allow_incremental_sync = false
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
    'rust', 'lua', 'python'
  },
  init_options = {
    linters = {
      flake8 = {
        command = vim.fn.expand('$HOME/.pyenv/shims/flake8'),
        rootPatterns = {".git"},
        debounce = 100,
        args = {'--format=%(row)d,%(col)d,%(code).1s,%(code)s: %(text)s', '-'},
        sourceName = 'flake8',
        offsetLint = 0,
        offsetColumn = 0,
        formatLines = 1,
        formatPattern = {
          '(\\d+),(\\d+),([A-Z]),(.*)(\\r|\\n)*$',
          {line = 1, column = 2, security = 3, message = {'[flake8]', 4}}
        },
        securities = {
          W = 'warning',
          E = 'error',
          F = 'error',
          C = 'error',
          N = 'error'
        }
      },
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
        sourceName = "golangci-lint",
        rootPatterns = {'.git', 'go.mod'},
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
      go = 'golangci',
      python = 'flake8'
    }
  }
}

-- config that activates keymaps and enables snippet support
local make_config = function()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.documentationFormat =
      {'markdown'}
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities.textDocument.completion.completionItem.preselectSupport = true
  capabilities.textDocument.completion.completionItem.insertReplaceSupport =
      true
  capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
  capabilities.textDocument.completion.completionItem.deprecatedSupport = true
  capabilities.textDocument.completion.completionItem.commitCharactersSupport =
      true
  capabilities.textDocument.completion.completionItem.tagSupport =
      {valueSet = {1}}
  capabilities.textDocument.completion.completionItem.resolveSupport =
      {properties = {'documentation', 'detail', 'additionalTextEdits'}}
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
      -- config.flags.debounce_text_changes = 1000
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
      update_in_insert = false
    })

