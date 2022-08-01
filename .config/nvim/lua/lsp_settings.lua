local vim = vim
local lspconfig = require "lspconfig"
local util = require("lspconfig").util
local ih = require("inlay-hints")

local set_diagnostic_sign = function()
  local signs = { "", "", "", "" }
  local diagnostic_types = { "Error", "Warn", "Info", "Hint" } -- or Warning, Information (which also don't seem to work) ...
  for i = 1, #diagnostic_types do
    local diagnostic_type = string.format("DiagnosticSign%s", diagnostic_types[i])
    local opts = {
      text = signs[i],
      texthl = string.format("DiagnosticSign%s", diagnostic_types[i]),
      linehl = "",
      numhl = "",
    }
    vim.fn.sign_define(diagnostic_type, opts)
  end
end

local custom_init = function(client)
  if client.config.flags then
    client.config.flags.allow_incremental_sync = true
  end
end

local custom_flags = { debounce_text_changes = 300 }

local custom_attach = function(client, bufnr)
  -- Set autocommands conditional on server_capabilities
  -- See https://github.com/neovim/nvim-lspconfig/wiki/UI-Customization#highlight-symbol-under-cursor
  if client.server_capabilities.documentHighlightProvider then
    vim.api.nvim_create_augroup("lsp_document_highlight", {
      clear = false,
    })
    vim.api.nvim_clear_autocmds {
      buffer = bufnr,
      group = "lsp_document_highlight",
    }
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
      group = "lsp_document_highlight",
      buffer = bufnr,
      callback = vim.lsp.buf.document_highlight,
    })
    vim.api.nvim_create_autocmd("CursorMoved", {
      group = "lsp_document_highlight",
      buffer = bufnr,
      callback = vim.lsp.buf.clear_references,
    })
  end

  -- See https://github.com/jose-elias-alvarez/null-ls.nvim/wiki/Avoiding-LSP-formatting-conflicts
  -- I only want to use null-ls formatting
  client.server_capabilities.document_formatting = false
  client.server_capabilities.document_range_formatting = false

  ih.on_attach(client, bufnr)
end

-- Configure lua language server for neovim development
local lua_config = {
  settings = {
    Lua = {
      runtime = {
        -- LuaJIT in the case of Neovim
        version = "LuaJIT",
        path = vim.split(package.path, ";"),
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      hint = {
        enable = true,
      },
      workspace = {
        maxPreload = 2000,
        preloadFileSize = 1000,
        --   -- Make the server aware of Neovim runtime files
        --   library = {
        --     [vim.fn.expand('$VIMRUNTIME/lua')] = true,
        --     [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true
        --   }
      },
    },
  },
}

local jsonls_config = {
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = true,
      format = { enable = false },
    },
  },
}
local yamlls_config = {
  settings = {
    yaml = {
      validate = true,
      format = { enable = false },
      hover = true,
      schemaStore = {
        enable = true,
        url = "https://www.schemastore.org/api/json/catalog.json",
      },
      schemaDownload = { enable = true },
      schemas = require("schemastore").json.schemas(),
      -- schemas = {}
    },
  },
}

local dart_config = {
  init_options = {
    closingLabels = true,
    flutterOutline = true,
    onlyAnalyzeProjectsWithOpenFiles = true,
    outline = true,
    suggestFromUnimportedLibraries = true,
  },
}

local gopls_config = {
  settings = {
    gopls = {
      hints = {
        assignVariableTypes = true,
        compositeLiteralFields = true,
        compositeLiteralTypes = true,
        constantValues = true,
        functionTypeParameters = true,
        parameterNames = true,
        rangeVariableTypes = true,
      },
    },
  },
  init_options = {
    gofumpt = true,
    usePlaceholders = false,
    semanticTokens = false,
    staticcheck = false,
    experimentalPostfixCompletions = true,
    analyses = {
      nilness = false,
      shadow = false,
      unusedparams = false,
      unusedwrite = false,
      fieldalignment = false,
    },
    -- matcher = "CaseSensitive",
    codelenses = { gc_details = true, tidy = true },
  },
}

-- local golangci_lint_ls_config = {
--   init_options = {
--     command = { "golangci-lint", "run", "--out-format", "json" },
--   },
-- }

local solargraph_config = {
  settings = {
    solargraph = {
      diagnostics = false,
      useBundler = false,
    },
  },
}

local pyright_config = {
  root_dir = function(fname)
    local root_files = {
      ".git",
      ".env",
      "venv",
      ".venv",
      "pyproject.toml",
      "pyrightconfig.json",
    }
    return util.root_pattern(unpack(root_files))(fname) or util.find_git_ancestor(fname) or util.path.dirname(fname)
  end,
}

local eslint_config = {
  settings = {
    foramt = {
      enable = false,
    },
  },
}

local function tsserver_organize_imports()
  local params = {
    command = "_typescript.organizeImports",
    arguments = { vim.api.nvim_buf_get_name(0) },
    title = "",
  }
  vim.lsp.buf.execute_command(params)
end

local tsserver_config = {
  settings = {
    javascript = {
      inlayHints = {
        includeInlayEnumMemberValueHints = true,
        includeInlayFunctionLikeReturnTypeHints = true,
        includeInlayFunctionParameterTypeHints = true,
        includeInlayParameterNameHints = "all", -- 'none' | 'literals' | 'all';
        includeInlayParameterNameHintsWhenArgumentMatchesName = true,
        includeInlayPropertyDeclarationTypeHints = true,
        includeInlayVariableTypeHints = true,
      },
    },
    typescript = {
      inlayHints = {
        includeInlayEnumMemberValueHints = true,
        includeInlayFunctionLikeReturnTypeHints = true,
        includeInlayFunctionParameterTypeHints = true,
        includeInlayParameterNameHints = "all", -- 'none' | 'literals' | 'all';
        includeInlayParameterNameHintsWhenArgumentMatchesName = true,
        includeInlayPropertyDeclarationTypeHints = true,
        includeInlayVariableTypeHints = true,
      },
    },
  },
  commands = {
    OrganizeImports = {
      tsserver_organize_imports,
      description = "Organize Imports",
    },
  },
}

local setup_lsp_ui = function()
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })
  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })
  vim.diagnostic.config {
    underline = true,
    virtual_text = { spacing = 2, prefix = "»", source = "if_many" },
    float = {
      source = false,
      border = "rounded",
      format = function(diagnostic)
        return diagnostic.message
      end,
      prefix = function(diagnostic)
        return string.format("%s(%s): ", diagnostic.source, diagnostic.code)
      end,
    },
    signs = { priority = 20 },
    update_in_insert = false,
  }
end

-- config that activates keymaps and enables snippet support
local make_config = function()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities.textDocument.completion.completionItem.preselectSupport = true
  capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
  capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
  capabilities.textDocument.completion.completionItem.deprecatedSupport = true
  capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
  capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
  capabilities.textDocument.completion.completionItem.resolveSupport = {
    properties = { "documentation", "detail", "additionalTextEdits" },
  }
  return {
    -- enable snippet support
    capabilities = capabilities,
    -- map buffer local keybindings when the language server attaches
    on_attach = custom_attach,
    on_init = custom_init,
    flags = custom_flags,
  }
end

local setup_servers = function()
  local lsp_installer = require "nvim-lsp-installer"

  local servers = lsp_installer.get_installed_servers()
  for _, server in ipairs(servers) do
    local opts = {}
    local config = make_config()

    local name = server.name
    if name == "sumneko_lua" then
      config.settings = lua_config.settings
    elseif name == "gopls" then
      config.init_options = gopls_config.init_options
      config.settings = gopls_config.settings
    -- elseif name == "golangci_lint_ls" then
    --   config.init_options = golangci_lint_ls_config.init_options
    elseif name == "pyright" then
      config.root_dir = pyright_config.root_dir
    elseif name == "solargraph" then
      config.settings = solargraph_config.settings
    elseif name == "jsonls" then
      config.settings = jsonls_config.settings
    elseif name == "yamlls" then
      config = require("yaml-companion").setup()
    elseif name == "dartls" then
      config.init_options = dart_config.init_options
    elseif name == "tsserver" then
      config.commands = tsserver_config.commands
      config.settings = tsserver_config.settings
    elseif name == "eslint" then
      -- フォーマットはprettierに寄せるので使用しない
      -- config.on_attach = function(client, bufnr)
      --   custom_attach(client, bufnr)
      --   -- neovim's LSP client does not currently support dynamic capabilities registration, so we need to set
      --   -- the resolved capabilities of the eslint server ourselves!
      --   -- client.server_capabilities.document_formatting = true
      -- end
      config.settings = eslint_config.settings
    end

    -- This setup() function is exactly the same as lspconfig's setup function (:help lspconfig-quickstart)
    opts = config
    lspconfig[name].setup(opts)
  end
end

setup_servers()
set_diagnostic_sign()
setup_lsp_ui()
