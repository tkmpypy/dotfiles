local vim = vim
local lspconfig = require "lspconfig"
local util = require("lspconfig").util

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

local custom_flags = function()
  return { debounce_text_changes = 300 }
end

local custom_attach = function(client, bufnr)
  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    local gid = vim.api.nvim_create_augroup("tkmpypy_lsp_doc_hi", { clear = true })
    vim.api.nvim_create_autocmd({ "CursorHold" }, {
      group = gid,
      buffer = bufnr,
      callback = function()
        vim.lsp.buf.document_highlight()
      end,
    })
    vim.api.nvim_create_autocmd({ "CursorMoved" }, {
      group = gid,
      buffer = bufnr,
      callback = function()
        vim.lsp.buf.clear_references()
      end,
    })
  end

  -- local gid = vim.api.nvim_create_augroup("tkmpypy_lsp_diagnostic", {clear=true})
  --   vim.api.nvim_create_autocmd({"CursorHold"}, {
  --     group = gid,
  --     buffer = bufnr,
  --     callback = function()
  --       vim.diagnostic.open_float(nil, {focus=false, scope="cursor"})
  --     end
  --   })

  -- See https://github.com/jose-elias-alvarez/null-ls.nvim/wiki/Avoiding-LSP-formatting-conflicts
  -- I only want to use null-ls formatting
  client.resolved_capabilities.document_formatting = false
  client.resolved_capabilities.document_range_formatting = false
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
    },
  },
}
local yamlls_config = {
  settings = {
    yaml = {
      schemas = require("schemastore").json.schemas(),
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
    flags = custom_flags(),
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
    elseif name == "pyright" then
      config.root_dir = pyright_config.root_dir
    elseif name == "jsonls" then
      config.settings = jsonls_config.settings
    elseif name == "yamlls" then
      config.settings = yamlls_config.settings
    elseif name == "dartls" then
      config.init_options = dart_config.init_options
    elseif name == "tsserver" then
      config.commands = tsserver_config.commands
    elseif name == "eslint" then
      -- フォーマットはprettierに寄せるので使用しない
      -- config.on_attach = function(client, bufnr)
      --   custom_attach(client, bufnr)
      --   -- neovim's LSP client does not currently support dynamic capabilities registration, so we need to set
      --   -- the resolved capabilities of the eslint server ourselves!
      --   -- client.resolved_capabilities.document_formatting = true
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
