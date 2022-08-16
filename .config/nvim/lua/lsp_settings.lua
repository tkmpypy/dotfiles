local vim = vim
local lspconfig = require "lspconfig"
local util = require("lspconfig").util
-- local ih = require("inlay-hints")
local navic = require "nvim-navic"

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
    navic.attach(client, bufnr)

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

  -- ih.on_attach(client, bufnr)
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
  local default_conf = make_config()
  require("mason-lspconfig").setup_handlers {
    -- The first entry (without a key) will be the default handler
    -- and will be called for each installed server that doesn't have
    -- a dedicated handler.
    function(server_name) -- default handler (optional)
      lspconfig[server_name].setup{}
    end,
    -- Next, you can provide targeted overrides for specific servers.
    ["sumneko_lua"] = function()
      local config = default_conf
      config.settings = lua_config.settings
      lspconfig.sumneko_lua.setup (config)
    end,
    ["gopls"] = function()
      local config = default_conf
      config.init_options = gopls_config.init_options
      config.settings = gopls_config.settings
      lspconfig.gopls.setup(config)
    end,
    ["pyright"] = function()
      local config = default_conf
      config.root_dir = pyright_config.root_dir
      lspconfig.pyright.setup(config)
    end,
    ["solargraph"] = function()
      local config = default_conf
      config.settings = solargraph_config.settings
      lspconfig.solargraph.setup(config)
    end,
    ["jsonls"] = function()
      local config = default_conf
      config.settings = jsonls_config.settings
      lspconfig.jsonls.setup(config)
    end,
    ["yamlls"] = function()
      local config = default_conf
      config.settings = yamlls_config.settings
      lspconfig.yamlls.setup(config)
    end,
    ["tsserver"] = function()
      local config = default_conf
      config.commands = tsserver_config.commands
      config.settings = tsserver_config.settings
      lspconfig.tsserver.setup(config)
    end,
    ["eslint"] = function()
      local config = default_conf
      config.settings = eslint_config.settings
      lspconfig.eslint.setup(config)
    end,
  }
end

setup_servers()
set_diagnostic_sign()
setup_lsp_ui()
