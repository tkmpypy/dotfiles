local vim = vim
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
    client.config.flags.allow_incremental_sync = false
  end
end

local custom_flags = function()
  return { debounce_text_changes = 300 }
end

local custom_attach = function(client, bufnr)
  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec(
      [[
    augroup lsp_document_highlight
    autocmd! * <buffer>
    autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
    autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
    augroup END
    ]],
      false
    )
  end

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
    usePlaceholders = true,
    semanticTokens = true,
    staticcheck = false,
    experimentalPostfixCompletions = true,
    analyses = {
      nilness = true,
      shadow = true,
      unusedparams = true,
      unusedwrite = true,
      fieldalignment = true,
    },
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

local function setup_servers_without_installer()
  require("lspconfig").dartls.setup {
    init_options = dart_config.init_options
  }
end

local function setup_servers_use_nvim_lsp_installer()
  local lsp_installer = require "nvim-lsp-installer"

  lsp_installer.on_server_ready(function(server)
    local opts = {}
    local config = make_config()

    -- (optional) Customize the options passed to the server
    -- if server.name == "tsserver" then
    --     opts.root_dir = function() ... end
    -- end
    if server.name == "sumneko_lua" then
      config.settings = lua_config.settings
    elseif server.name == "gopls" then
      config.init_options = gopls_config.init_options
    elseif server.name == "pyright" then
      config.root_dir = pyright_config.root_dir
    elseif server.name == "jsonls" then
      config.settings = jsonls_config.settings
    elseif server.name == "dartls" then
      config.init_options = dart_config.init_options
    end

    -- This setup() function is exactly the same as lspconfig's setup function (:help lspconfig-quickstart)
    opts = config
    server:setup(opts)
    vim.cmd [[ do User LspAttachBuffers ]]
  end)
end

setup_servers_without_installer()
setup_servers_use_nvim_lsp_installer()
require("lspconfig")["null-ls"].setup {}
set_diagnostic_sign()

vim.diagnostic.config({
    underline = true,
    virtual_text = { spacing = 2, prefix = "»" },
    signs = { priority = 20 },
    update_in_insert = false,
})
