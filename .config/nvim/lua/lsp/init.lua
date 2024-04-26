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

  if client.supports_method("textDocument/inlayHint") then
    set_inlay_hint_hl()
  end
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

-- local golangci_lint_ls_config = {
--   init_options = {
--     command = { "golangci-lint", "run", "--out-format", "json" },
--   },
-- }

local solargraph_config = {
  settings = {
    solargraph = {
      diagnostics = true,
      useBundler = false,
    },
  },
}

local sqlls_config = {
  root_dir = function(fname)
    local root_files = {
      ".git",
    }
    return util.root_pattern(unpack(root_files))(fname) or util.find_git_ancestor(fname) or util.path.dirname(fname)
  end,
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

local tailwindcss_config = {
  filetypes = {
    "aspnetcorerazor",
    "astro",
    "astro-markdown",
    "blade",
    -- "clojure",
    "django-html",
    "htmldjango",
    "edge",
    "eelixir",
    "elixir",
    "ejs",
    "erb",
    "eruby",
    -- "gohtml",
    -- "gohtmltmpl",
    "haml",
    "handlebars",
    "hbs",
    "html",
    "html-eex",
    "heex",
    "jade",
    "leaf",
    "liquid",
    -- "markdown",
    -- "mdx",
    "mustache",
    "njk",
    "nunjucks",
    -- "php",
    "razor",
    "slim",
    "twig",
    "css",
    "less",
    "postcss",
    "sass",
    "scss",
    "stylus",
    "sugarss",
    "javascript",
    "javascriptreact",
    "reason",
    "rescript",
    "typescript",
    "typescriptreact",
    "vue",
    "svelte",
  },
  settings = {
    tailwindCSS = {
      classAttributes = { "class", "className", "class:list", "classList", "ngClass" },
      lint = {
        cssConflict = "warning",
        invalidApply = "error",
        invalidConfigPath = "error",
        invalidScreen = "error",
        invalidTailwindDirective = "error",
        invalidVariant = "error",
        recommendedVariantOrder = "warning",
      },
      validate = true,
    },
  },
}

local setup_lsp_ui = function()
  vim.diagnostic.config({
    warden = {
      line_highlight = true,
    },
    underline = true,
    -- virtual_text = { spacing = 2, source = "if_many" },
    virtual_text = { spacing = 0, severity = { min = vim.diagnostic.severity.HINT } },
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

local setup_servers = function()
  require("mason-lspconfig").setup_handlers({
    -- The first entry (without a key) will be the default handler
    -- and will be called for each installed server that doesn't have
    -- a dedicated handler.
    function(server_name) -- default handler (optional)
      lspconfig[server_name].setup(make_default_config())
    end,
    -- Next, you can provide targeted overrides for specific servers.
    ["lua_ls"] = function()
      local config = make_default_config()
      config.settings = lua_config.settings
      lspconfig.lua_ls.setup(config)
    end,
    ["gopls"] = function()
      local config = make_default_config()
      config.settings = gopls_config.settings
      config.init_options = gopls_config.init_options
      lspconfig.gopls.setup(config)
    end,
    ["rust_analyzer"] = function()
      -- local config = make_config()
      -- config.settings = rust_config.settings
      -- lspconfig.rust_analyzer.setup(config)
      local rt = require("rust-tools")

      rt.setup({
        server = {
          on_attach = custom_attach,
          settings = rust_config.settings,
        },
        tools = { -- rust-tools options
          -- automatically call RustReloadWorkspace when writing to a Cargo.toml file.
          reload_workspace_from_cargo_toml = true,

          -- These apply to the default RustSetInlayHints command
          inlay_hints = {
            -- automatically set inlay hints (type hints)
            -- default: true
            auto = false,

            -- Only show inlay hints for the current line
            only_current_line = false,

            -- whether to show parameter hints with the inlay hints or not
            -- default: true
            show_parameter_hints = true,

            -- prefix for parameter hints
            -- default: "<-"
            parameter_hints_prefix = "<- ",

            -- prefix for all the other hints (type, chaining)
            -- default: "=>"
            other_hints_prefix = "=> ",

            -- whether to align to the length of the longest line in the file
            max_len_align = false,

            -- padding from the left if max_len_align is true
            max_len_align_padding = 1,

            -- whether to align to the extreme right or not
            right_align = false,

            -- padding from the right if right_align is true
            right_align_padding = 7,

            -- The color of the hints
            highlight = "Comment",
          },

          -- options same as lsp hover / vim.lsp.util.open_floating_preview()
          hover_actions = {

            -- the border that is used for the hover window
            -- see vim.api.nvim_open_win()
            border = {
              { "╭", "FloatBorder" },
              { "─", "FloatBorder" },
              { "╮", "FloatBorder" },
              { "│", "FloatBorder" },
              { "╯", "FloatBorder" },
              { "─", "FloatBorder" },
              { "╰", "FloatBorder" },
              { "│", "FloatBorder" },
            },

            -- whether the hover action window gets automatically focused
            -- default: false
            auto_focus = false,
          },
        },
      })
    end,
    ["sqlls"] = function()
      local config = make_default_config()
      config.root_dir = sqlls_config.root_dir
      lspconfig.sqlls.setup(config)
    end,
    ["pyright"] = function()
      local config = make_default_config()
      config.root_dir = pyright_config.root_dir
      lspconfig.pyright.setup(config)
    end,
    ["solargraph"] = function()
      local config = make_default_config()
      config.settings = solargraph_config.settings
      lspconfig.solargraph.setup(config)
    end,
    ["html"] = function()
      local config = make_default_config()
      config = vim.tbl_deep_extend("force", config, html_config)
      lspconfig.html.setup(config)
    end,
    ["cssls"] = function()
      local config = make_default_config()
      config = vim.tbl_deep_extend("force", config, cssls_config)
      lspconfig.cssls.setup(config)
    end,
    ["jsonls"] = function()
      local config = make_default_config()
      config.settings = jsonls_config.settings
      lspconfig.jsonls.setup(config)
    end,
    ["tailwindcss"] = function()
      local config = make_default_config()
      config = vim.tbl_deep_extend("force", config, tailwindcss_config)
      lspconfig.tailwindcss.setup(config)
    end,
    ["yamlls"] = function()
      local config = make_default_config()
      local yaml_config = require("yaml-companion").setup()
      config = vim.tbl_deep_extend("force", config, yaml_config)
      lspconfig.yamlls.setup(config)
    end,
    ["tsserver"] = function()
      local config = make_default_config()
      config.commands = tsserver_config.commands
      config.settings = tsserver_config.settings
      lspconfig.tsserver.setup(config)
    end,
    ["vtsls"] = function()
      local config = make_default_config()
      config.settings = vtsls_config.settings
      lspconfig.vtsls.setup(config)
    end,
    ["eslint"] = function()
      local config = make_default_config()
      config.settings = eslint_config.settings
      lspconfig.eslint.setup(config)
    end,
  })
end

setup_lsp_ui()
setup_servers()
set_diagnostic_sign()
