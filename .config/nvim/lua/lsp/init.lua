local exists_neodev, neodev = pcall(require, "neodev")
if exists_neodev then
  neodev.setup({
    library = {
      enabled = true, -- when not enabled, lua-dev will not change any settings to the LSP server
      -- these settings will be used for your Neovim config directory
      runtime = true, -- runtime path
      types = true, -- full signature, docs and completion of vim.api, vim.treesitter, vim.lsp and others
      plugins = true,
      -- you can also specify the list of plugins to make available as a workspace library
      -- plugins = { "nvim-treesitter", "plenary.nvim", "telescope.nvim" },
    },
    setup_jsonls = false, -- configures jsonls to provide completion for project specific .luarc.json files
    -- override = function(root_dir, library)
    --   library.enabled = true
    --   library.plugins = true
    --   -- library.runtime = true
    --   -- library.types = true
    --   secrity
    -- end,
    lspconfig = true,
    pathStrict = true,
    debug = true,
  })
end

local vim = vim

local lspconfig = require("lspconfig")
local util = require("lspconfig").util

local function set_inlay_hint_hl()
  local has_hl, hl = pcall(vim.api.nvim_get_hl, 0, { name = "LspInlayHint", link = true })
  if has_hl and (hl["foreground"] or hl["background"]) then
    return
  end

  hl = vim.api.nvim_get_hl(0, { name = "Comment", link = true })
  local foreground = string.format("#%06x", hl["foreground"] or 0)
  if #foreground < 3 then
    foreground = ""
  end

  hl = vim.api.nvim_get_hl(0, { name = "CursorLine", link = true })
  local background = string.format("#%06x", hl["background"] or 0)
  if #background < 3 then
    background = ""
  end

  vim.api.nvim_set_hl(0, "LspInlayHint", { fg = foreground, bg = background })
end

local set_diagnostic_sign = function()
  -- local signs = { "", "", "", "󰌶" }
  -- local signs = { "", "", "", "" }
  vim.diagnostic.config({
    signs = {
      text = {
        [vim.diagnostic.severity.ERROR] = "",
        [vim.diagnostic.severity.WARN] = "",
        [vim.diagnostic.severity.HINT] = "",
        [vim.diagnostic.severity.INFO] = "",
      },
    },
  })
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
    vim.api.nvim_clear_autocmds({
      buffer = bufnr,
      group = "lsp_document_highlight",
    })
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
      group = "lsp_document_highlight",
      buffer = bufnr,
      callback = vim.lsp.buf.document_highlight,
    })
    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
      group = "lsp_document_highlight",
      buffer = bufnr,
      callback = function()
        vim.lsp.buf.clear_references(bufnr)
      end,
    })
  end

  if client:supports_method("textDocument/inlayHint") then
    set_inlay_hint_hl()
  end

  -- LSPのフォーマットを無効化する
  client.server_capabilities.documentFormattingProvider = false
end

-- Configure lua language server for neovim development
-- https://github.com/sumneko/lua-language-server/wiki/Settings
local lua_config = {
  settings = {
    Lua = {
      format = {
        enable = false,
        -- Put format options here
        -- NOTE: the value should be STRING!!
        defaultConfig = {
          indent_style = "space",
          indent_size = "2",
        },
      },
      completion = {
        callSnippet = "Replace",
      },

      -- https://github.com/sumneko/lua-language-server/wiki/Diagnostics
      diagnostics = {
        disable = {
          "missing-parameter",
        },
      },
      workspace = {
        checkThirdParty = false, -- THIS IS THE IMPORTANT LINE TO ADD
      },
      hint = {
        enable = true,
      },
    },
  },
}

local cssls_config = {
  settings = {
    css = {
      validate = true,
    },
    scss = {
      validate = true,
    },
    less = {
      validate = true,
    },
  },
  single_file_support = true,
}

local jsonls_config = {
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = {
        enable = true,
      },
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
        enable = false,
        url = "",
      },
      schemaDownload = { enable = true },
      schemas = require("schemastore").yaml.schemas(),
      -- schemas = {}
    },
  },
}

local rust_config = {
  settings = {
    ["rust-analyzer"] = {
      check = {
        command = "clippy",
      },
      inlayHints = { locationLinks = false },
    },
  },
}

local gopls_config = {
  init_options = {
    usePlaceholders = true,
  },
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
      gofumpt = true,
      semanticTokens = true,
      staticcheck = false,
      experimentalPostfixCompletions = true,
      analyses = {
        nilness = false,
        shadow = false,
        unusedparams = false,
        unusedwrite = false,
        fieldalignment = false,
      },
      allowModfileModifications = true,
      -- matcher = "CaseSensitive",
      codelenses = {
        gc_details = true,
        tidy = true,
        generate = true,
        regenerate_cgo = true,
        run_govulncheck = true,
        test = true,
        upgrade_dependency = true,
        vendor = true,
      },
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

local intelephense_config = {
  settings = {
    intelephense = {
      environment = {
        phpVersion = "8.2",
      },
      diagnostics = {
        enable = true,
        undefinedTypes = false,
        undefinedFunctions = false,
        undefinedConstants = false,
        undefinedClassConstants = false,
        undefinedMethods = false,
        undefinedProperties = false,
        undefinedVariables = false,
      },
    },
  },
}

local phpactor_config = {
  -- cmd = { "phpactor", "language-server" },
  init_options = {
    ["language_server_phpstan.enabled"] = false,
    ["language_server_psalm.enabled"] = false,
    -- ["php.version"] = "8.2",
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

local vtsls_config = {
  settings = {
    typescript = {
      format = {
        enable = false,
      },
      suggest = {
        completeFunctionCalls = true,
      },
      updateImportsOnFileMove = "always",
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
    javascript = {
      format = {
        enable = false,
      },
      suggest = {
        completeFunctionCalls = true,
      },
      updateImportsOnFileMove = "always",
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
}

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

local html_config = {
  filetypes = {
    "html",
    "blade",
  },
}

local setup_lsp_ui = function()
  vim.diagnostic.config({
    warden = {
      line_highlight = true,
    },
    underline = true,
    virtual_text = true,
    virtual_lines = false,
    signs = true,
    severity_sort = true,
    float = {
      source = true,
      border = "rounded",
      format = function(diagnostic)
        local content = diagnostic.message
        local href = nil
        local lsp = vim.tbl_get(diagnostic, "user_data", "lsp")
        if lsp ~= nil then
          local _href = vim.tbl_get(lsp, "codeDescription", "href")
          if _href ~= nil then
            href = _href
          end

          local rendered = vim.tbl_get(lsp, "data", "rendered")
          if rendered ~= nil then
            content = rendered
          end
        end
        if href ~= nil then
          return string.format("%s\n   %s", content, href)
        end

        return string.format("%s", content)
      end,
    },
    signs = { priority = 20 },
    update_in_insert = false,
  })
end

-- config that activates keymaps and enables snippet support
local make_default_config = function()
  local capabilities = {
    textDocument = {
      completion = {
        dynamicRegistration = false,
        completionItem = {
          snippetSupport = true,
          commitCharactersSupport = true,
          deprecatedSupport = true,
          preselectSupport = true,
          insertReplaceSupport = true,
          resolveSupport = {
            properties = {
              "documentation",
              "detail",
              "additionalTextEdits",
              "sortText",
              "filterText",
              "insertText",
              "textEdit",
              "insertTextFormat",
              "insertTextMode",
            },
          },
          insertTextModeSupport = {
            valueSet = {
              1, -- asIs
              2, -- adjustIndentation
            },
          },
          labelDetailsSupport = true,
        },
        contextSupport = true,
        insertTextMode = 1,
        completionList = {
          itemDefaults = {
            "commitCharacters",
            "editRange",
            "insertTextFormat",
            "insertTextMode",
            "data",
          },
        },
      },
    },
  }
  capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true,
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

local server_list = {
  ["lua_ls"] = {
    config = lua_config,
  },
  ["gopls"] = {
    config = gopls_config,
  },
  ["pyright"] = {
    config = pyright_config,
  },
  ["intelephense"] = {
    config = intelephense_config,
  },
  ["vtsls"] = {
    config = vtsls_config,
  },
  ["html"] = {
    config = html_config,
  },
  ["cssls"] = {
    config = cssls_config,
  },
  ["jsonls"] = {
    config = jsonls_config,
  },
  ["yamlls"] = {
    config = yamlls_config,
  },
  ["copilot"] = {
    config = nil,
  },
}

local setup_servers = function()
  vim.lsp.handlers["workspace/diagnostic/refresh"] = function(_, _, ctx)
    local ns = vim.lsp.diagnostic.get_namespace(ctx.client_id)
    pcall(vim.diagnostic.reset, ns)
    return true
  end

  local default_config = make_default_config()
  if vim.g.complete_engine_type == "blink" then
    default_config.capabilities = require("blink.cmp").get_lsp_capabilities(default_config.capabilities)
  elseif vim.g.complete_engine_type == "cmp" then
    local cmp_capabilities = require("cmp_nvim_lsp").default_capabilities()
    default_config.capabilities = vim.tbl_deep_extend("force", default_config.capabilities, cmp_capabilities)
  end

  vim.lsp.config("*", default_config)
  for server, value in pairs(server_list) do
    if value.config ~= nil then
      vim.lsp.config(server, value.config)
    end

    vim.lsp.enable(server)
  end

  vim.lsp.inline_completion.enable()
end

local setup_keymap = function()
  vim.keymap.set("i", "<C-x>", function()
    vim.lsp.inline_completion.get()
  end)
end

setup_lsp_ui()
setup_servers()
setup_keymap()
set_diagnostic_sign()
