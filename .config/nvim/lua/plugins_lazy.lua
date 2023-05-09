-- bootstraping for lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "--single-branch",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    event = "BufReadPre",
    cmd = { "TSUpdate" },
    enabled = function()
      return vim.g.use_treesitter
    end,
    build = function()
      require("nvim-treesitter.install").update({ with_sync = true })
    end,
    config = function()
      require("nvim-treesitter.configs").setup({
        highlight = {
          enable = true,
          disable = function(lang, buf)
            local max_filesize = 200 * 1024 -- 200 KB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
              return true
            end
            return false
          end,
          additional_vim_regex_highlighting = true,
        },
        yati = {
          enable = true,
          default_lazy = true,
        },
        indent = {
          enable = false,
        },
        -- Install parsers synchronously (only applied to `ensure_installed`)
        sync_install = false,

        -- Automatically install missing parsers when entering buffer
        auto_install = false,
        ensure_installed = {
          "java",
          "dart",
          "go",
          "rust",
          "ruby",
          "python",
          "lua",
          "yaml",
          "toml",
          "json",
          "typescript",
          "javascript",
          "tsx",
          "html",
          "vim",
          "markdown",
          "markdown_inline",
          "regex",
          "make",
          "dockerfile",
          "proto",
        },
        -- textobjects = {
        --   move = {
        --     enable = true,
        --     set_jumps = true, -- whether to set jumps in the jumplist
        --   },
        --   select = {
        --     enable = true,
        --     -- Automatically jump forward to textobj, similar to targets.vim
        --     lookahead = true,
        --     include_surrounding_whitespace = true,
        --   },
        -- },
        endwise = {
          enable = true,
        },
        autotag = {
          enable = true,
        },
        rainbow = {
          enable = true,
          extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
        },
        context_commentstring = { enable = true, enable_autocmd = false },
      })
    end,
  },
  {
    "yioneko/nvim-yati",
    event = "BufReadPre",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },
  {
    "RRethy/nvim-treesitter-endwise",
    event = "BufReadPre",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },
  {
    "windwp/nvim-ts-autotag",
    event = "BufReadPre",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },
  {
    "mrjones2014/nvim-ts-rainbow",
    event = "BufReadPre",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    event = "BufReadPre",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },
  {
    "b3nj5m1n/kommentary",
    event = "BufReadPre",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      local config = require("kommentary.config")
      config.use_extended_mappings()
      config.configure_language("default", {
        ignore_whitespace = true,
        use_consistent_indentation = true,
        prefer_single_line_comments = true,
      })
    end,
  },
  {
    "danymat/neogen",
    event = "BufReadPre",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      require("neogen").setup({
        enabled = true,
        snippet_engine = "luasnip",
        languages = {
          go = {
            template = {
              annotation_convention = "godoc",
            },
          },
          lua = {
            template = {
              annotation_convention = "emmylua",
            },
          },
          python = {
            template = {
              annotation_convention = "google_docstrings",
            },
          },
        },
      })
    end,
  },
  {
    "lukas-reineke/headlines.nvim",
    ft = { "markdown" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      vim.cmd([[highlight Headline1 guibg=#1e2718]])
      vim.cmd([[highlight Headline2 guibg=#000a40]])
      vim.cmd([[highlight Headline3 guibg=#32063e]])
      vim.cmd([[highlight Headline4 guibg=#430000]])
      vim.cmd([[highlight Headline5 guibg=#21262d]])
      vim.cmd([[highlight CodeBlock guibg=#1c1c1c]])
      vim.cmd([[highlight Dash guibg=#D19A66 gui=bold]])
      require("headlines").setup({
        markdown = {
          headline_highlights = { "Headline1", "Headline2", "Headline3", "Headline4", "Headline5" },
        },
      })
    end,
  },

  -- Language
  {
    "hashivim/vim-terraform",
    ft = "terraform",
  },
  {
    "uarun/vim-protobuf",
    ft = "proto",
  },
  {
    "towolf/vim-helm",
    ft = { "helm", "yaml" },
    config = function()
      vim.cmd([[autocmd BufRead,BufNewFile */templates/*.yml,*/templates/*.yaml,*/templates/*.tpl set ft=helm]])
    end,
  },
  {
    "aklt/plantuml-syntax",
    ft = "plantuml",
  },

  -- runner
  {
    "vim-test/vim-test",
    cmd = {
      "TestNearest",
      "TestFile",
      "TestSuite",
      "TestLast",
    },
    config = function()
      vim.cmd([[
        let g:test#echo_command = 1
        let g:test#runner_commands = ['PyUnit']
        let g:test#strategy = "neovim"
        let g:test#neovim#term_position = "belowright"
        let g:test#preserve_screen = 1
        let g:test#javascript#runner = 'jest'
        let g:test#javascript#jest#executable = './node_modules/.bin/jest'
        let g:test#python#runner = 'pytest'
        let g:test#python#pytest#options = {
            \ 'nearest': '-vv --capture=no',
            \ 'file': '-vv --capture=no'
        \ }

        let g:test#rust#cargotest#options = {
            \ 'nearest': '-- --nocapture'
        \ }
        let g:test#rust#cargonextest#options = {
            \ 'nearest': '--no-capture'
        \ }
        let g:test#go#gotest#options = {
            \ 'nearest': '-v'
        \ }
      ]])
    end,
  },
  {
    "thinca/vim-quickrun",
    cmd = "QuickRun",
    config = function()
      vim.cmd([[
        let g:quickrun_config = {}
        let g:quickrun_config['typescript'] = { 'type' : 'typescript/tsc' }
        let g:quickrun_config['typescript/tsc'] = {
        \   'command': 'tsc',
        \   'exec': ['%c --target esnext --module commonjs %o %s', 'node %s:r.js'],
        \   'tempfile': '%{tempname()}.ts',
        \   'hook/sweep/files': ['%S:p:r.js'],
        \ }
        let g:quickrun_config['rust'] = { 'type' : 'rust/cargo' }
        let g:quickrun_config['rust/cargo'] = {
        \   'command': 'cargo',
        \   'exec': ['%c run'],
        \ }
      ]])
    end,
  },

  -- UI
  {
    "nvim-tree/nvim-web-devicons",
    lazy = true,
  },
  {
    "rcarriga/nvim-notify",
    lazy = true,
    config = function()
      require("notify").setup({
        background_colour = "#000000",
      })
    end,
  },
  {
    "nvim-zh/colorful-winsep.nvim",
    event = "ColorScheme",
    config = function()
      require("colorful-winsep").setup({
        enable = true,
        -- Window divider color definition
        highlight = {
          fg = "#957CC6",
        },
        -- filetype in the list, will not be executed
        no_exec_files = {
          "packer",
          "TelescopePrompt",
          "mason",
          "CompetiTest",
          "NvimTree",
          "NeogitStatus",
          "NeogitCommitMessage",
        },
        create_event = function() end,
        close_event = function() end,
      })
    end,
  },
  {
    "levouh/tint.nvim",
    event = "BufReadPre",
    enabled = false,
    config = function()
      local ignore_ft = { "aerial" }
      require("tint").setup({
        tint = -45, -- Darken colors, use a positive value to brighten
        saturation = 0.6, -- Saturation to preserve
        transforms = require("tint").transforms.SATURATE_TINT, -- Showing default behavior, but value here can be predefined set of transforms
        tint_background_colors = false, -- Tint background portions of highlight groups
        highlight_ignore_patterns = { "WinSeparator", "Status.*" }, -- Highlight group patterns to ignore, see `string.find`
        window_ignore_function = function(winid)
          local bufid = vim.api.nvim_win_get_buf(winid)
          local buftype = vim.api.nvim_buf_get_option(bufid, "buftype")
          local ft = vim.api.nvim_buf_get_option(bufid, "filetype")
          local floating = vim.api.nvim_win_get_config(winid).relative ~= ""

          if vim.tbl_contains(ignore_ft, ft) then
            return true
          end

          -- Do not tint `terminal` or floating windows, tint everything else
          return buftype == "terminal" or floating
        end,
      })
    end,
  },
  {
    "petertriho/nvim-scrollbar",
    dependencies = { "kevinhwang91/nvim-hlslens" },
    event = { "BufReadPre" },
    config = function()
      require("hlslens").setup()
      require("scrollbar").setup({
        show = true,
        set_highlights = true,
        handle = {
          text = " ",
          color = nil,
          cterm = nil,
          highlight = "CursorColumn",
          hide_if_all_visible = true, -- Hides handle if all lines are visible
        },
        marks = {
          Search = {
            text = { "-", "=" },
            priority = 0,
            color = nil,
            cterm = nil,
            highlight = "Search",
          },
          Error = {
            text = { "-", "=" },
            priority = 1,
            color = nil,
            cterm = nil,
            highlight = "DiagnosticVirtualTextError",
          },
          Warn = {
            text = { "-", "=" },
            priority = 2,
            color = nil,
            cterm = nil,
            highlight = "DiagnosticVirtualTextWarn",
          },
          Info = {
            text = { "-", "=" },
            priority = 3,
            color = nil,
            cterm = nil,
            highlight = "DiagnosticVirtualTextInfo",
          },
          Hint = {
            text = { "-", "=" },
            priority = 4,
            color = nil,
            cterm = nil,
            highlight = "DiagnosticVirtualTextHint",
          },
          Misc = {
            text = { "-", "=" },
            priority = 5,
            color = nil,
            cterm = nil,
            highlight = "Normal",
          },
        },
        excluded_buftypes = {
          "terminal",
        },
        excluded_filetypes = {
          "prompt",
          "TelescopePrompt",
          "noice",
        },
        autocmd = {
          render = {
            "BufWinEnter",
            "TabEnter",
            "TermEnter",
            "WinEnter",
            "CmdwinLeave",
            "TextChanged",
            "VimResized",
            "WinScrolled",
          },
        },
        handlers = {
          diagnostic = true,
          search = true, -- Requires hlslens to be loaded, will run require("scrollbar.handlers.search").setup() for you
        },
      })
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPre" },
    config = function()
      -- vim.cmd [[highlight IndentBlanklineIndent1 guifg=#E06C75 blend=nocombine]]
      -- vim.cmd [[highlight IndentBlanklineIndent2 guifg=#E5C07B blend=nocombine]]
      -- vim.cmd [[highlight IndentBlanklineIndent3 guifg=#98C379 blend=nocombine]]
      -- vim.cmd [[highlight IndentBlanklineIndent4 guifg=#56B6C2 blend=nocombine]]
      -- vim.cmd [[highlight IndentBlanklineIndent5 guifg=#61AFEF blend=nocombine]]
      -- vim.cmd [[highlight IndentBlanklineIndent6 guifg=#C678DD blend=nocombine]]
      -- vim.cmd [[highlight IndentBlanklineIndent1 guibg=#1f1f1f blend=nocombine]]
      -- vim.cmd [[highlight IndentBlanklineIndent2 guibg=#1a1a1a blend=nocombine]]
      require("indent_blankline").setup({
        -- char = "",
        enabled = true,
        buftype_exclude = { "terminal", "help", "nofile" },
        filetype_exclude = { "startify", "alpha", "NvimTree", "neo-tree" },
        show_end_of_line = false,
        -- space_char_blankline = " ",
        show_trailing_blankline_indent = false,
        -- char_highlight_list = {
        --     "IndentBlanklineIndent1",
        --     "IndentBlanklineIndent2",
        --     "IndentBlanklineIndent3",
        --     "IndentBlanklineIndent4",
        --     "IndentBlanklineIndent5",
        --     "IndentBlanklineIndent6",
        -- },
        show_current_context = true,
        show_current_context_start = false,
      })
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    event = { "VeryLazy" },
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local util = require("scripts/util")
      local lualine_utils = require("lualine.utils.utils")
      local theme = require("lualine/themes/onedark")
      local lsp_status = function()
        if vim.g.lsp_client_type == "neovim" then
          return util.lsp.current_lsp
        elseif vim.g.lsp_client_type == "coc" then
          return "g:coc_status"
        end
      end
      require("lualine").setup({
        options = {
          icons_enabled = true,
          theme = "auto",
          -- component_separators = { left = "", right = "" },
          -- section_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
          component_separators = "",
          -- section_separators = "",
          disabled_filetypes = {},
          always_divide_middle = true,
          globalstatus = true,
        },
        sections = {
          lualine_a = { "mode" },
          lualine_b = {
            {
              "filetype",
              icon_only = true,
            },
            {
              "filename",
              symbols = { modified = "  ", readonly = "  ", unnamed = "  " },
              path = 1,
            },
          },
          lualine_c = {
            {
              lsp_status(),
              icon = " ",
              color = {
                fg = theme.normal.a.bg,
                gui = "bold",
              },
            },
            {
              "diagnostics",
              always_visible = true,
              diagnostics_color = {
                error = {
                  fg = lualine_utils.extract_color_from_hllist("fg", {
                    "DiagnosticError",
                    "DiagnosticSignError",
                    "LspDiagnosticsDefaultError",
                    "DiffDelete",
                    "ErrorText",
                  }, "#e32636"),
                },
                warn = {
                  fg = lualine_utils.extract_color_from_hllist("fg", {
                    "DiagnosticWarn",
                    "DiagnosticSignWarn",
                    "LspDiagnosticsDefaultWarning",
                    "DiffText",
                    "WarningText",
                  }, "#ffa500"),
                },
                info = {
                  fg = lualine_utils.extract_color_from_hllist("fg", {
                    "DiagnosticInfo",
                    "DiagnosticSignInfo",
                    "LspDiagnosticsDefaultInformation",
                    "Normal",
                    "InfoText",
                  }, "#ffffff"),
                },
                hint = {
                  fg = lualine_utils.extract_color_from_hllist("fg", {
                    "DiagnosticHint",
                    "DiagnosticSignHint",
                    "LspDiagnosticsDefaultHint",
                    "DiffChange",
                    "HintText",
                  }, "#273faf"),
                },
              },
            },
          },
          lualine_x = {
            {
              "branch",
              icon = "",
              color = {
                fg = theme.visual.a.bg,
              },
            },
            {
              "diff",
              -- Is it me or the symbol for modified us really weird
              symbols = { added = " ", modified = "柳", removed = " " },
            },
          },
          lualine_y = { "encoding" },
          lualine_z = { "selectioncount", "location" },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { "filename" },
          lualine_x = { "selectioncount", "location" },
          lualine_y = {},
          lualine_z = {},
        },
        tabline = {},
        extensions = {
          "aerial",
          "fzf",
          "quickfix",
          "toggleterm",
          "symbols-outline",
          "nvim-tree",
          "neo-tree",
          "lazy",
        },
      })
    end,
  },
  {
    {
      "glepnir/dashboard-nvim",
      event = "VimEnter",
      config = function()
        require("dashboard").setup({
          -- config
          theme = "hyper",
          config = {
            week_header = {
              enable = true,
            },
            shortcut = {
              { desc = "󰊳 Update", group = "@property", action = "Lazy update", key = "u" },
              {
                icon = " ",
                icon_hl = "@variable",
                desc = "Find Files",
                group = "Label",
                action = "Telescope find_files",
                key = "F",
              },
              {
                icon = " ",
                desc = "Explore Files",
                group = "Number",
                action = "Neotree toggle",
                key = "f",
              },
            },
          },
        })
      end,
      dependencies = { { "nvim-tree/nvim-web-devicons" } },
    },
  },
  {
    "goolord/alpha-nvim",
    event = "VimEnter",
    enabled = function()
      return vim.g.splash_type == "alpha-nvim"
    end,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local alpha = require("alpha")
      local theme = require("alpha.themes.theta")
      local dashboard = require("alpha.themes.dashboard")

      -- Set header
      local header = {
        type = "text",
        val = {
          [[                                                     ]],
          [[  ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗ ]],
          [[  ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║ ]],
          [[  ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║ ]],
          [[  ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║ ]],
          [[  ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║ ]],
          [[  ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝ ]],
          [[                                                     ]],
        },
        opts = {
          position = "center",
          hl = "Type",
        },
      }
      local buttons = {
        type = "group",
        val = {
          { type = "text", val = "Quick links", opts = { hl = "SpecialComment", position = "center" } },
          { type = "padding", val = 1 },
          dashboard.button("e", "  New file", "<cmd>ene<CR>"),
          dashboard.button("SPC s f f", "  Find file"),
          dashboard.button("SPC s g g", "  Live grep"),
          dashboard.button("U", "  Update plugins", "<cmd>Lazy sync<CR>"),
          dashboard.button("q", "  Quit", "<cmd>qa<CR>"),
        },
        position = "center",
      }
      theme.header.val = header.val
      theme.header.opts = header.opts
      theme.buttons.val = buttons.val
      alpha.setup(theme.config)
      vim.cmd([[
            autocmd FileType alpha setlocal nofoldenable
        ]])
    end,
  },
  {
    "stevearc/aerial.nvim",
    cmd = "AerialToggle",
    config = function()
      require("aerial").setup({
        attach_mode = "global", -- "window" or "global"
        layout = {
          placement = "window", -- "window" or "edge"
        },
      })
    end,
  },
  {
    "stevearc/dressing.nvim",
    lazy = true,
    opts = {
      input = {
        insert_only = true,
        relative = "win",
        prefer_width = 60,
        min_width = { 60, 0.6 },
      },
    },
    init = function()
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.select = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.select(...)
      end
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.input = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.input(...)
      end
    end,
  },
  {
    "folke/noice.nvim",
    dependencies = {
      "MunifTanjim/nui.nvim",
      "rcarriga/nvim-notify",
    },
    event = "VeryLazy",
    config = function()
      require("noice").setup({
        presets = {
          bottom_search = true, -- use a classic bottom cmdline for search
          command_palette = false, -- position the cmdline and popupmenu together
          long_message_to_split = false, -- long messages will be sent to a split
          inc_rename = true, -- enables an input dialog for inc-rename.nvim
          lsp_doc_border = true, -- add a border to hover docs and signature help
        },
        cmdline = {
          view = "cmdline_popup", -- view for rendering the cmdline. Change to `cmdline` to get a classic cmdline at the bottom
          opts = { buf_options = { filetype = "vim" } }, -- enable syntax highlighting in the cmdline
        },
        messages = {
          -- NOTE: If you enable messages, then the cmdline is enabled automatically.
          -- This is a current Neovim limitation.
          enabled = true, -- enables the Noice messages UI
          view = "mini", -- default view for messages
        },
        popupmenu = {
          enabled = true, -- enables the Noice popupmenu UI
          ---@type 'nui'|'cmp'
          backend = "cmp", -- backend to use to show regular cmdline completions
        },
        history = {
          -- options for the message history that you get with `:Noice`
          view = "split",
          opts = { enter = true },
          filter = { event = { "msg_show", "notify" }, ["not"] = { kind = { "search_count", "echo" } } },
        },
        notify = {
          enabled = true,
        },
        lsp = {
          progress = {
            enabled = false,
          },
          override = {
            -- override the default lsp markdown formatter with Noice
            ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            -- override the lsp markdown formatter with Noice
            ["vim.lsp.util.stylize_markdown"] = true,
            -- override cmp documentation with Noice (needs the other options to work)
            ["cmp.entry.get_documentation"] = false,
          },
          hover = {
            enabled = true,
          },
          signature = {
            enabled = true,
            auto_open = {
              enabled = true,
              trigger = true, -- Automatically show signature help when typing a trigger character from the LSP
              luasnip = true, -- Will open signature help when jumping to Luasnip insert nodes
              throttle = 50, -- Debounce lsp signature help request by 50ms
            },
          },
          message = {
            -- Messages shown by lsp servers
            enabled = true,
            view = "notify",
          },
          -- defaults for hover and signature help
          documentation = {
            view = "hover",
            opts = {
              lang = "markdown",
              replace = true,
              render = "plain",
              format = { "{message}" },
              win_options = { concealcursor = "n", conceallevel = 3 },
            },
          },
        },
      })
      vim.keymap.set({ "n", "i", "s" }, "<c-d>", function()
        if not require("noice.lsp").scroll(4) then
          return "<c-f>"
        end
      end, { silent = true, expr = true })

      vim.keymap.set({ "n", "i", "s" }, "<c-u>", function()
        if not require("noice.lsp").scroll(-4) then
          return "<c-b>"
        end
      end, { silent = true, expr = true })
    end,
  },

  -- explorer
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v2.x",
    event = "VeryLazy",
    cmd = {"Neotree"},
    enabled = function()
      return vim.g.file_explorer_type == "neo-tree"
    end,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
      {
        -- only needed if you want to use the commands with "_with_window_picker" suffix
        "s1n7ax/nvim-window-picker",
        config = function()
          require("window-picker").setup({
            autoselect_one = true,
            include_current = false,
            filter_rules = {
              -- filter using buffer options
              bo = {
                -- if the file type is one of following, the window will be ignored
                filetype = { "neo-tree", "neo-tree-popup", "notify" },

                -- if the buffer type is one of following, the window will be ignored
                buftype = { "terminal", "quickfix" },
              },
            },
            other_win_hl_color = "#e35e4f",
          })
        end,
      },
    },
    config = function()
      -- Unless you are still migrating, remove the deprecated commands from v1.x
      vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])

      require("neo-tree").setup({
        close_if_last_window = false, -- Close Neo-tree if it is the last window left in the tab
        popup_border_style = "rounded",
        enable_git_status = true,
        enable_diagnostics = true,
        open_files_do_not_replace_types = { "terminal", "trouble", "qf" }, -- when opening files, do not use windows containing these filetypes or buftypes
        sort_case_insensitive = false, -- used when sorting files and directories in the tree
        sort_function = nil, -- use a custom function for sorting files and directories in the tree
        -- sort_function = function (a,b)
        --       if a.type == b.type then
        --           return a.path > b.path
        --       else
        --           return a.type > b.type
        --       end
        --   end , -- this sorts files and directories descendantly
        default_component_configs = {
          container = {
            enable_character_fade = true,
          },
          indent = {
            indent_size = 2,
            padding = 1, -- extra padding on left hand side
            -- indent guides
            with_markers = true,
            indent_marker = "│",
            last_indent_marker = "└",
            highlight = "NeoTreeIndentMarker",
            -- expander config, needed for nesting files
            with_expanders = nil, -- if nil and file nesting is enabled, will enable expanders
            expander_collapsed = "",
            expander_expanded = "",
            expander_highlight = "NeoTreeExpander",
          },
          icon = {
            folder_closed = "",
            folder_open = "",
            folder_empty = "ﰊ",
            -- The next two settings are only a fallback, if you use nvim-web-devicons and configure default icons there
            -- then these will never be used.
            default = "*",
            highlight = "NeoTreeFileIcon",
          },
          modified = {
            symbol = "  ",
            highlight = "NeoTreeModified",
          },
          name = {
            trailing_slash = false,
            use_git_status_colors = true,
            highlight = "NeoTreeFileName",
          },
          git_status = {
            symbols = {
              -- Change type
              added = "✚", -- or "✚", but this is redundant info if you use git_status_colors on the name
              modified = "", -- or "", but this is redundant info if you use git_status_colors on the name
              deleted = "✖", -- this can only be used in the git_status source
              renamed = "", -- this can only be used in the git_status source
              -- Status type
              untracked = "",
              ignored = "",
              unstaged = "",
              staged = "",
              conflict = "",
            },
          },
        },
        window = {
          position = "left",
          width = 30,
          mapping_options = {
            noremap = true,
            nowait = true,
          },
          mappings = {
            ["<space>"] = false,
            ["<2-LeftMouse>"] = "open",
            -- ["<cr>"] = "open",
            ["<cr>"] = "open_with_window_picker",
            ["<esc>"] = "revert_preview",
            ["P"] = { "toggle_preview", config = { use_float = true } },
            ["l"] = "focus_preview",
            -- ["<C-x>"] = "open_split",
            -- ["<C-v>"] = "open_vsplit",
            ["<C-x>"] = "split_with_window_picker",
            ["<C-v>"] = "vsplit_with_window_picker",
            ["t"] = "open_tabnew",
            -- ["<cr>"] = "open_drop",
            -- ["t"] = "open_tab_drop",
            ["w"] = "open_with_window_picker",
            --["P"] = "toggle_preview", -- enter preview mode, which shows the current node without focusing
            ["C"] = "close_node",
            -- ['C'] = 'close_all_subnodes',
            ["z"] = "close_all_nodes",
            --["Z"] = "expand_all_nodes",
            ["n"] = {
              "add",
              -- this command supports BASH style brace expansion ("x{a,b,c}" -> xa,xb,xc). see `:h neo-tree-file-actions` for details
              -- some commands may take optional config options, see `:h neo-tree-mappings` for details
              config = {
                show_path = "relative", -- "none", "relative", "absolute"
              },
            },
            ["N"] = "add_directory", -- also accepts the optional config.show_path option like "add". this also supports BASH style brace expansion.
            ["d"] = "delete",
            ["r"] = "rename",
            ["y"] = "copy_to_clipboard",
            ["x"] = "cut_to_clipboard",
            ["p"] = "paste_from_clipboard",
            ["c"] = "copy", -- takes text input for destination, also accepts the optional config.show_path option like "add":
            -- ["c"] = {
            --  "copy",
            --  config = {
            --    show_path = "none" -- "none", "relative", "absolute"
            --  }
            --}
            ["m"] = "move", -- takes text input for destination, also accepts the optional config.show_path option like "add".
            ["q"] = "close_window",
            ["R"] = "refresh",
            ["?"] = "show_help",
            ["<"] = "prev_source",
            [">"] = "next_source",
          },
        },
        nesting_rules = {},
        filesystem = {
          filtered_items = {
            visible = false, -- when true, they will just be displayed differently than normal items
            hide_dotfiles = false,
            hide_gitignored = false,
            hide_hidden = false, -- only works on Windows for hidden files/directories
            hide_by_name = {
              "node_modules",
            },
            hide_by_pattern = { -- uses glob style patterns
              --"*.meta",
              --"*/src/*/tsconfig.json",
            },
            always_show = { -- remains visible even if other settings would normally hide it
              --".gitignored",
            },
            never_show = { -- remains hidden even if visible is toggled to true, this overrides always_show
              ".DS_Store",
              "thumbs.db",
            },
            never_show_by_pattern = { -- uses glob style patterns
              --".null-ls_*",
            },
          },
          follow_current_file = false, -- This will find and focus the file in the active buffer every
          -- time the current file is changed while the tree is open.
          group_empty_dirs = false, -- when true, empty folders will be grouped together
          hijack_netrw_behavior = "disabled", -- netrw disabled, opening a directory opens neo-tree
          -- in whatever position is specified in window.position
          -- "open_current",  -- netrw disabled, opening a directory opens within the
          -- window like netrw would, regardless of window.position
          -- "disabled",    -- netrw left alone, neo-tree does not handle opening dirs
          use_libuv_file_watcher = true, -- This will use the OS level file watchers to detect changes
          -- instead of relying on nvim autocmd events.
          window = {
            mappings = {
              ["u"] = "navigate_up",
              ["."] = "set_root",
              ["H"] = "toggle_hidden",
              ["/"] = "fuzzy_finder",
              ["D"] = "fuzzy_finder_directory",
              ["#"] = "fuzzy_sorter", -- fuzzy sorting using the fzy algorithm
              -- ["D"] = "fuzzy_sorter_directory",
              ["f"] = "filter_on_submit",
              ["<c-q>"] = "clear_filter",
              ["[g"] = "prev_git_modified",
              ["]g"] = "next_git_modified",
            },
          },
        },
        buffers = {
          follow_current_file = false, -- This will find and focus the file in the active buffer every
          -- time the current file is changed while the tree is open.
          group_empty_dirs = true, -- when true, empty folders will be grouped together
          show_unloaded = true,
          window = {
            mappings = {
              ["bd"] = "buffer_delete",
              ["u"] = "navigate_up",
              ["."] = "set_root",
            },
          },
        },
        git_status = {
          window = {
            position = "float",
            mappings = {
              ["A"] = "git_add_all",
              ["gu"] = "git_unstage_file",
              ["ga"] = "git_add_file",
              ["gr"] = "git_revert_file",
              ["gc"] = "git_commit",
              ["gp"] = "git_push",
              ["gg"] = "git_commit_and_push",
            },
          },
        },
      })
    end,
  },
  {
    "nvim-tree/nvim-tree.lua",
    event = "VeryLazy",
    enabled = function()
      return vim.g.file_explorer_type == "nvim-tree"
    end,
    config = function()
      require("nvim-tree").setup({
        auto_reload_on_write = true,
        -- disables netrw completely
        disable_netrw = true,
        -- hijack netrw window on startup
        hijack_netrw = false,
        -- open the tree when running this setup function
        open_on_setup = false,
        -- will not open on setup if the filetype is in this list
        ignore_ft_on_setup = {},
        -- opens the tree when changing/opening a new tab if the tree wasn't previously opened
        open_on_tab = false,
        -- hijack the cursor in the tree to put it at the start of the filename
        hijack_cursor = false,
        -- updates the root directory of the tree on `DirChanged` (when your run `:cd` usually)
        update_cwd = false,
        -- update the focused file on `BufEnter`, un-collapses the folders recursively until it finds the file
        update_focused_file = {
          -- enables the feature
          enable = false,
          -- update the root directory of the tree to the one of the folder containing the file if the file is not under the current root directory
          -- only relevant when `update_focused_file.enable` is true
          update_cwd = false,
          -- list of buffer names / filetypes that will not update the cwd if the file isn't found under the current root directory
          -- only relevant when `update_focused_file.update_cwd` is true and `update_focused_file.enable` is true
          ignore_list = {},
        },
        -- show lsp diagnostics in the signcolumn
        diagnostics = {
          enable = false,
          icons = {
            hint = "",
            info = "",
            warning = "",
            error = "",
          },
        },
        -- configuration options for the system open command (`s` in the tree by default)
        system_open = {
          -- the command to run this, leaving nil should work in most cases
          cmd = nil,
          -- the command arguments as a list
          args = {},
        },

        filters = {
          dotfiles = false,
          -- custom = { "node_modules", ".cache", ".DS_Store" },
          custom = { ".cache", ".DS_Store" },
        },
        git = {
          enable = true,
          ignore = false,
          timeout = 500,
        },

        view = {
          -- width of the window, can be either a number (columns) or a string in `%`
          width = 30,
          -- side of the tree, can be one of 'left' | 'right' | 'top' | 'bottom'
          side = "left",
          preserve_window_proportions = false,
          mappings = {
            -- custom only false will merge the list with the default mappings
            -- if true, it will only use your list to set the mappings
            custom_only = true,
            -- list of mappings to set on the tree manually
            list = {
              { key = "n", mode = "n", action = "create" },
              { key = "u", mode = "n", action = "dir_up" },
              { key = "-", action = "dir_up" },
              { key = "<CR>", action = "edit" },
              { key = "o", mode = "n", action = "cd" },
              { key = "<C-v>", action = "vsplit" },
              { key = "<C-x>", action = "split" },
              { key = "<C-t>", action = "tabnew" },
              { key = "<", action = "prev_sibling" },
              { key = ">", action = "next_sibling" },
              { key = "P", action = "parent_node" },
              { key = "<BS>", action = "close_node" },
              { key = "<S-CR>", action = "close_node" },
              { key = "<Tab>", action = "preview" },
              { key = "K", action = "first_sibling" },
              { key = "J", action = "last_sibling" },
              { key = "I", action = "toggle_ignored" },
              { key = "H", action = "toggle_dotfiles" },
              { key = "R", action = "refresh" },
              { key = "D", action = "remove" },
              { key = "d", action = "trash" },
              { key = "r", action = "rename" },
              { key = "<C->", action = "full_rename" },
              { key = "x", action = "cut" },
              { key = "c", action = "copy" },
              { key = "p", action = "paste" },
              { key = "y", action = "copy_name" },
              { key = "Y", action = "copy_path" },
              { key = "gy", action = "copy_absolute_path" },
              { key = "[c", action = "prev_git_item" },
              { key = "}c", action = "next_git_item" },
              { key = "<C-k>", action = "toggle_file_info" },
              { key = "q", action = "close" },
              { key = "?", action = "toggle_help" },
            },
          },
          float = {
            enable = false,
            quit_on_focus_loss = true,
            open_win_config = {
              relative = "editor",
              border = "rounded",
              width = 30,
              height = 30,
              row = 1,
              col = 1,
            },
          },
        },
        renderer = {
          highlight_opened_files = "none",
          indent_markers = {
            enable = false,
            icons = {
              corner = "└ ",
              edge = "│ ",
              none = "  ",
            },
          },
          highlight_git = true,
        },
        notify = {
          threshold = vim.log.levels.WARN,
        },
      })
    end,
  },

  -- Lua Utils
  {
    "rafcamlet/nvim-luapad",
    cmd = "Luapad",
  },

  -- Operator
  {
    "kana/vim-operator-replace",
    event = { "BufReadPre" },
    dependencies = { "kana/vim-operator-user" },
  },

  -- Utils
  {
    "epwalsh/obsidian.nvim",
    cmd = {
      "ObsidianOpen",
      "ObsidianNew",
      "ObsidianToday",
      "ObsidianYesterday",
      "ObsidianLink",
      "ObsidianFollowLink",
      "ObsidianSearch",
      "ObsidianQuickSwitch",
      "ObsidianLinkNew",
    },
    opts = {
      dir = "~/Dropbox/notes",
      daily_notes = {
        folder = "journal",
      },
      completion = {
        nvim_cmp = true, -- if using nvim-cmp, otherwise set to false
      },
      note_id_func = function(title)
        -- Create note IDs in a Zettelkasten format with a timestamp and a suffix.
        local suffix = ""
        if title ~= nil then
          -- If title is given, transform it into valid file name.
          suffix = title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
        else
          -- If title is nil, just add 4 random uppercase letters to the suffix.
          for _ = 1, 4 do
            suffix = suffix .. string.char(math.random(65, 90))
          end
        end
        return tostring(os.date("%Y-%m-%d")) .. "-" .. suffix
      end,
      use_advanced_uri = false,
    },
  },
  {
    "monaqa/dial.nvim",
    config = function()
      local augend = require("dial.augend")
      require("dial.config").augends:register_group({
        -- default augends used when no group name is specified
        default = {
          augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
          augend.integer.alias.hex, -- nonnegative hex number  (0x01, 0x1a1f, etc.)
          augend.date.alias["%Y/%m/%d"], -- date (2022/02/19, etc.)
          augend.constant.alias.bool,
          -- uppercase hex number (0x1A1A, 0xEEFE, etc.)
          augend.constant.new({
            elements = { "and", "or" },
            word = true, -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
            cyclic = true, -- "or" is incremented into "and".
          }),
          augend.constant.new({
            elements = { "&&", "||" },
            word = false,
            cyclic = true,
          }),
          augend.case.new({
            types = { "PascalCase", "camelCase", "snake_case", "kebab-case", "SCREAMING_SNAKE_CASE" },
            cyclic = true,
          }),
        },
      })
    end,
    keys = {
      {
        "<C-a>",
        function()
          return require("dial.map").inc_normal()
        end,
        mode = "n",
        noremap = true,
        silent = true,
        expr = true,
      },
      {
        "<C-x>",
        function()
          return require("dial.map").dec_normal()
        end,
        mode = "n",
        noremap = true,
        silent = true,
        expr = true,
      },
      {
        "<C-a>",
        function()
          return require("dial.map").inc_visual()
        end,
        mode = "v",
        noremap = true,
        silent = true,
        expr = true,
      },
      {
        "<C-x>",
        function()
          return require("dial.map").dec_visual()
        end,
        mode = "v",
        noremap = true,
        silent = true,
        expr = true,
      },
      {
        "g<C-a>",
        function()
          return require("dial.map").inc_gvisual()
        end,
        mode = "v",
        noremap = true,
        silent = true,
        expr = true,
      },
      {
        "g<C-x>",
        function()
          return require("dial.map").dec_gvisual()
        end,
        mode = "v",
        noremap = true,
        silent = true,
        expr = true,
      },
    },
  },
  {
    "uga-rosa/ccc.nvim",
    event = { "BufReadPre" },
    config = function()
      local ccc = require("ccc")
      ccc.setup({
        highlighter = {
          auto_enable = true,
          max_byte = 200 * 1024,
          lsp = true,
        },
      })
    end,
  },
  {
    "thinca/vim-qfreplace",
    cmd = "Qfreplace",
  },
  {
    "haya14busa/vim-asterisk",
    event = { "BufReadPre" },
    config = function()
      vim.cmd([[
          let g:asterisk#keeppos = 1
        ]])
      vim.keymap.set("", "*", "<Plug>(asterisk-z*)", {})
      vim.keymap.set("", "#", "<Plug>(asterisk-z#)", {})
      vim.keymap.set("", "g*", "<Plug>(asterisk-g*)", {})
      vim.keymap.set("", "g#", "<Plug>(asterisk-g#)", {})
      vim.keymap.set("", "z*", "<Plug>(asterisk-z*)", {})
      vim.keymap.set("", "gz*", "<Plug>(asterisk-gz*)", {})
      vim.keymap.set("", "z#", "<Plug>(asterisk-z#)", {})
      vim.keymap.set("", "gz#", "<Plug>(asterisk-gz#)", {})
    end,
  },
  {
    "akinsho/toggleterm.nvim",
    config = function()
      require("toggleterm").setup({
        size = 20,
        open_mapping = [[<c-t>]],
        shade_filetypes = {},
        shade_terminals = false,
        direction = "float",
        insert_mappings = true,
        close_on_exit = false,
        persist_size = false,
        persist_mode = false,
      })
    end,
  },
  {
    "ojroques/nvim-bufdel",
    config = function()
      require("bufdel").setup({
        next = "alternative", -- or 'cycle, 'alternate'
        quit = false, -- quit Neovim when last buffer is closed
      })
    end,
    keys = {
      {
        "<leader>q",
        "<cmd>BufDel<CR>",
        mode = "n",
        desc = "Delete buffer",
      },
      {
        "<leader>Q",
        "<cmd>BufDel!<CR>",
        mode = "n",
        desc = "Delete buffer and ignore changes",
      },
    },
  },
  {
    "godlygeek/tabular",
    enabled = false,
  },
  {
    "airblade/vim-rooter",
    config = function()
      vim.g.rooter_patterns = { ".git", "Cargo.toml", "package.json" }
    end,
  },
  "machakann/vim-sandwich",
  {
    "simeji/winresizer",
  },
  {
    "windwp/nvim-autopairs",
    event = { "InsertEnter", "CmdlineEnter" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    config = function()
      local npairs = require("nvim-autopairs")
      npairs.setup({
        map_cr = true,
      })
    end,
  },
  {
    "iamcco/markdown-preview.nvim",
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
    ft = { "markdown" },
  },
  { "npxbr/glow.nvim", ft = { "markdown" } },
  "osyo-manga/vim-over",
  {
    "phaazon/hop.nvim",
    keys = {
      {
        "f",
        "<cmd>lua require'hop'.hint_char1({ direction = nil, current_line_only = true })<cr>",
        mode = "n",
        desc = "hop hint char",
      },
      {
        "f",
        "<cmd>lua require'hop'.hint_char1({ direction = nil, current_line_only = true, inclusive_jump = true })<cr>",
        mode = "o",
        desc = "hop hint char(inclusive_jump)",
      },
      {
        "L",
        "<cmd>lua require'hop'.hint_lines_skip_whitespace({ current_line_only = false })<cr>",
        mode = { "n", "v" },
        desc = "hop lines",
      },
      {
        "mc",
        "<cmd>lua require'hop'.hint_char1({ direction = nil, current_line_only = false })<cr>",
        mode = "n",
        desc = "hop hint char(current_line_only=false)",
      },
      {
        "mw",
        "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.BEGIN })<cr>",
        mode = { "n", "v" },
        desc = "hop hint words(BEGIN)",
      },
      {
        "mw",
        "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.BEGIN, inclusive_jump = true })<cr>",
        mode = "o",
        desc = "hop hint words(BEGIN)",
      },
      {
        "mW",
        "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END })<cr>",
        mode = { "n", "v" },
        desc = "hop hint words(END)",
      },
      {
        "mW",
        "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END, inclusive_jump = true })<cr>",
        mode = "o",
        desc = "hop hint words(END)",
      },
    },
    config = function()
      require("hop").setup({
        keys = "etovxqpdygfblzhckisuran",
        jump_on_sole_occurrence = false,
      })
    end,
  },
  {
    "mtdl9/vim-log-highlighting",
    ft = { "log" },
  },
  {
    "fannheyward/telescope-coc.nvim",
    enabled = function()
      return vim.g.lsp_client_type == "coc"
    end,
  },
  {
    "kevinhwang91/nvim-bqf",
    cmd = { "BqfEnable", "BqfDisable", "BqfToggle", "BqfAutoToggle" },
    ft = "qf",
    config = function()
      require("bqf").setup({
        auto_enable = true,
        auto_resize_height = true, -- highly recommended enable
        preview = {
          win_height = 12,
          win_vheight = 12,
          delay_syntax = 80,
          border_chars = { "┃", "┃", "━", "━", "┏", "┓", "┗", "┛", "█" },
          show_title = false,
        },
        -- make `drop` and `tab drop` to become preferred
        func_map = {
          drop = "o",
          openc = "O",
          split = "<C-s>",
          tabdrop = "<C-t>",
          -- set to empty string to disable
          tabc = "",
          ptogglemode = "z,",
        },
        filter = {
          fzf = {
            action_for = { ["ctrl-s"] = "split", ["ctrl-t"] = "tab drop" },
            extra_opts = { "--bind", "ctrl-o:toggle-all", "--prompt", "> " },
          },
        },
      })
    end,
  },
  {
    "nvim-telescope/telescope.nvim",
    lazy = true,
    cmd = {"Telescope"},
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
      },
    },
    config = function()
      local telescope = require("telescope")
      telescope.setup({
        defaults = {
          mappings = {
            i = {
              ["<C-h>"] = "which_key",
            },
          },
          vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
            "--hidden",
          },
          prompt_prefix = "  ",
          path_display = { "smart" },
          winblend = 10,
          scroll_strategy = "cycle",
          color_devicon = true,
          preview = {
            treesitter = false,
          },
          file_previewer = require("telescope.previewers").cat.new,
          grep_previewer = require("telescope.previewers").vimgrep.new,
          qflist_previewer = require("telescope.previewers").qflist.new,
        },
        pickers = {
          buffers = {
            -- sort_lastused = true,
            sort_mru = true,
            ignore_current_buffer = true,
            theme = "dropdown",
            -- previewer = true,
            mappings = {
              i = { ["<c-d>"] = require("telescope.actions").delete_buffer },
              n = { ["<c-d>"] = require("telescope.actions").delete_buffer },
            },
            find_files = {
              path_display = { "smart" },
            },
          },
        },
        extensions = {},
        fzf = {
          fuzzy = true, -- false will only do exact matching
          override_generic_sorter = true, -- override the generic sorter
          override_file_sorter = true, -- override the file sorter
          case_mode = "smart_case", -- or "ignore_case" or "respect_case"
          -- the default case_mode is "smart_case"
        },
      })
      telescope.load_extension("fzf")
      if vim.g.lsp_client_type == "coc" then
        telescope.load_extension("coc")
      end
    end,
  },

  -- Git
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("gitsigns").setup({
        numhl = true,
        linehl = false,
        current_line_blame = true,
        current_line_blame_formatter_opts = {
          relative_time = true,
        },
        current_line_blame_formatter = "   <author> • <summary>   <author_time:%R> at <author_time:%Y/%m/%d %H:%M>",
        signs = {
          add = { hl = "GitSignsAdd", text = "┃", numhl = "GitSignsAddNr", linehl = "GitSignsAddLn" },
          change = {
            hl = "GitSignsChange",
            text = "┃",
            numhl = "GitSignsChangeNr",
            linehl = "GitSignsChangeLn",
          },
          delete = {
            hl = "GitSignsDelete",
            text = "┃",
            numhl = "GitSignsDeleteNr",
            linehl = "GitSignsDeleteLn",
          },
          topdelete = {
            hl = "GitSignsDelete",
            text = "┃",
            numhl = "GitSignsDeleteNr",
            linehl = "GitSignsDeleteLn",
          },
          changedelete = {
            hl = "GitSignsChange",
            text = "┃",
            numhl = "GitSignsChangeNr",
            linehl = "GitSignsChangeLn",
          },
          untracked = {
            hl = "GitSignsAdd",
            text = "│",
            numhl = "GitSignsAddNr",
            linehl = "GitSignsAddLn",
          },
        },
      })
    end,
  },
  {
    "sindrets/diffview.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
    keys = {
      {
        "<leader>gdd",
        "<cmd>DiffviewOpen<CR>",
        mode = "n",
        noremap = true,
        silent = true,
        expr = true,
        desc = "Diffview Open",
      },
      {
        "<leader>gdr",
        "<cmd>DiffviewFileHistory .<CR>",
        mode = "n",
        noremap = true,
        silent = true,
        expr = true,
        desc = "Diffview current history",
      },
    },
    config = function()
      local actions = require("diffview.actions")
      require("diffview").setup({
        diff_binaries = false, -- Show diffs for binaries
        enhanced_diff_hl = false, -- See ':h diffview-config-enhanced_diff_hl'
        use_icons = true, -- Requires nvim-web-devicons
        icons = { -- Only applies when use_icons is true.
          folder_closed = "",
          folder_open = "",
        },
        signs = {
          fold_closed = "",
          fold_open = "",
        },
        file_panel = {
          listing_style = "tree", -- One of 'list' or 'tree'
          tree_options = { -- Only applies when listing_style is 'tree'
            flatten_dirs = true, -- Flatten dirs that only contain one single dir
            folder_statuses = "only_folded", -- One of 'never', 'only_folded' or 'always'.
          },
          win_config = { -- See ':h diffview-config-win_config'
            position = "left",
            width = 35,
          },
        },
        file_history_panel = {
          log_options = {
            git = {
              single_file = {
                max_count = 512,
                follow = true,
              },
              multi_file = {
                max_count = 128,
                -- follow = false   -- `follow` only applies to single-file history
              },
            },
          },
          win_config = { -- See ':h diffview-config-win_config'
            position = "bottom",
            height = 16,
          },
        },
        commit_log_panel = {
          win_config = {}, -- See ':h diffview-config-win_config'
        },
        default_args = { -- Default args prepended to the arg-list for the listed commands
          DiffviewOpen = {},
          DiffviewFileHistory = {},
        },
        hooks = {}, -- See ':h diffview-config-hooks'
        keymaps = {
          disable_defaults = false, -- Disable the default key bindings
          -- The `view` bindings are active in the diff buffers, only when the current
          -- tabpage is a Diffview.
          view = {
            ["q"] = actions.close,
          },
          file_panel = {
            ["q"] = actions.close,
          },
          file_history_panel = {
            ["q"] = actions.close,
          },
          option_panel = {
            ["q"] = actions.close,
          },
        },
      })
    end,
  },
  {
    "TimUntersberger/neogit",
    lazy = true,
    enabled = function()
      return vim.g.git_client_type == "neogit"
    end,
    dependencies = { "nvim-lua/plenary.nvim", "sindrets/diffview.nvim" },
    config = function()
      local neogit = require("neogit")
      neogit.setup({
        disable_signs = false,
        disable_context_highlighting = false,
        disable_commit_confirmation = true,
        auto_refresh = true,
        disable_builtin_notifications = false,
        -- customize displayed signs
        signs = {
          -- { CLOSED, OPENED }
          section = { "", "" },
          item = { "", "" },
          hunk = { "", "" },
        },
        integrations = {
          diffview = true,
        },
        -- override/add mappings
        mappings = {
          -- modify status buffer mappings
          status = {
            ["q"] = "Close",
            ["1"] = "Depth1",
            ["2"] = "Depth2",
            ["3"] = "Depth3",
            ["4"] = "Depth4",
            ["<tab>"] = "Toggle",
            ["x"] = "Discard",
            ["s"] = "Stage",
            ["S"] = "StageUnstaged",
            ["<c-s>"] = "StageAll",
            ["u"] = "Unstage",
            ["U"] = "UnstageStaged",
            ["d"] = "DiffAtFile",
            ["D"] = "DiffPopup",
            ["$"] = "CommandHistory",
            ["<c-r>"] = "RefreshBuffer",
            ["<enter>"] = "GoToFile",
            ["<c-v>"] = "VSplitOpen",
            ["<c-x>"] = "SplitOpen",
            ["<c-t>"] = "TabOpen",
            ["?"] = "HelpPopup",
            ["p"] = "PullPopup",
            ["r"] = "RebasePopup",
            ["P"] = "PushPopup",
            ["c"] = "CommitPopup",
            ["L"] = "LogPopup",
            ["Z"] = "StashPopup",
            ["b"] = "BranchPopup",
          },
        },
      })
    end,
  },

  -- Snippet
  "mattn/vim-sonictemplate",

  -- LSP
  {
    "j-hui/fidget.nvim",
    opts = {
      text = {
        spinner = "pipe", -- animation shown when tasks are ongoing
        done = "✔", -- character shown when all tasks are complete
        commenced = "Started", -- message shown when task starts
        completed = "Completed", -- message shown when task completes
      },
      align = {
        bottom = true, -- align fidgets along bottom edge of buffer
        right = true, -- align fidgets along right edge of buffer
      },
    },
  },
  {
    "williamboman/mason.nvim",
    cmd = { "Mason", "MasonLog", "MasonInstall", "MasonUninstall", "MasonUninstallAll" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    config = function()
      require("mason").setup({
        ui = {
          border = "rounded",
          icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗",
          },
        },
      })
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    event = { "BufReadPre" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "lua_ls",
          "rust_analyzer",
          "cssls",
          "dockerls",
          "eslint",
          "gopls",
          "html",
          "jsonls",
          "vtsls",
          -- "tsserver",
          "marksman",
          "pyright",
          "tailwindcss",
          "terraformls",
          "vimls",
          "yamlls",
        },
        -- Whether servers that are set up (via lspconfig) should be automatically installed if they're not already installed.
        -- This setting has no relation with the `ensure_installed` setting.
        -- Can either be:
        --   - false: Servers are not automatically installed.
        --   - true: All servers set up via lspconfig are automatically installed.
        --   - { exclude: string[] }: All servers set up via lspconfig, except the ones provided in the list, are automatically installed.
        --       Example: automatic_installation = { exclude = { "rust_analyzer", "solargraph" } }
        automatic_installation = false,
      })
    end,
  },
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    config = function()
      require("lsp_settings")
    end,
  },
  {
    "SmiteshP/nvim-navic",
    event = "BufReadPre",
    dependencies = "neovim/nvim-lspconfig",
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
  },
  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    version = "*",
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    event = "BufReadPre",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons", -- optional dependency
    },
    opts = {
      theme = "auto",
      context_follow_icon_color = false,
      exclude_filetypes = { "gitcommit", "toggleterm" },
      create_autocmd = true,
      attach_navic = true,
      symbols = {
        ---modification indicator
        ---@type string
        modified = "●",

        ---truncation indicator
        ---@type string
        ellipsis = "…",

        ---entry separator
        ---@type string
        separator = "",
      },
      -- kinds = {
      --   File = " ",
      --   Module = " ",
      --   Namespace = " ",
      --   Package = " ",
      --   Class = " ",
      --   Method = " ",
      --   Property = " ",
      --   Field = " ",
      --   Constructor = " ",
      --   Enum = "練",
      --   Interface = "練",
      --   Function = " ",
      --   Variable = " ",
      --   Constant = " ",
      --   String = " ",
      --   Number = " ",
      --   Boolean = "◩ ",
      --   Array = " ",
      --   Object = " ",
      --   Key = " ",
      --   Null = "ﳠ ",
      --   EnumMember = " ",
      --   Struct = " ",
      --   Event = " ",
      --   Operator = " ",
      --   TypeParameter = " ",
      -- },
      kinds = {
        File = "",
        Module = "",
        Namespace = "",
        Package = "",
        Class = "",
        Method = "",
        Property = "",
        Field = "",
        Constructor = "",
        Enum = "",
        Interface = "",
        Function = "",
        Variable = "",
        Constant = "",
        String = "",
        Number = "",
        Boolean = "",
        Array = "",
        Object = "",
        Key = "",
        Null = "",
        EnumMember = "",
        Struct = "",
        Event = "",
        Operator = "",
        TypeParameter = "",
      },
    },
  },
  {
    "folke/neodev.nvim",
    ft = "lua",
    dependencies = { "neovim/nvim-lspconfig" },
  },
  {
    "someone-stole-my-name/yaml-companion.nvim",
    ft = "yaml",
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "neovim/nvim-lspconfig",
    },
    config = function()
      require("telescope").load_extension("yaml_schema")
    end,
  },
  {
    "simrat39/rust-tools.nvim",
    ft = "rust",
    dependencies = { "neovim/nvim-lspconfig" },
  },
  {
    "glepnir/lspsaga.nvim",
    event = { "BufReadPre" },
    -- enabled = function()
    --   return vim.g.lsp_client_type == "neovim"
    -- end,
    enabled = false,
    dependencies = { "neovim/nvim-lspconfig" },
    config = function()
      require("lspsaga").setup({
        preview = {
          lines_above = 0,
          lines_below = 10,
        },
        scroll_preview = {
          scroll_down = "<C-f>",
          scroll_up = "<C-b>",
        },
        request_timeout = 2000,
        finder = {
          edit = { "o", "<CR>" },
          vsplit = "v",
          split = "s",
          tabe = "t",
          quit = { "q", "<ESC>" },
        },
        definition = {
          edit = "<CR>",
          vsplit = "<C-v>",
          split = "<C-s>",
          tabe = "<C-t>",
          quit = "q",
          close = "<Esc>",
        },
        code_action = {
          num_shortcut = true,
          keys = {
            -- string |table type
            quit = "q",
            exec = "<CR>",
          },
        },
        lightbulb = {
          enable = false,
        },
        diagnostic = {
          twice_into = false,
          show_code_action = true,
          show_source = true,
          keys = {
            exec_action = "o",
            quit = "q",
            go_action = "g",
          },
        },
        rename = {
          quit = "<C-c>",
          exec = "<CR>",
          mark = "x",
          confirm = "<CR>",
          in_select = false,
          whole_project = true,
        },
        symbol_in_winbar = {
          enable = true,
          separator = "  ",
          hide_keyword = true,
          show_file = true,
          folder_level = 2,
          respect_root = false,
          color_mode = true,
        },
        ui = {
          -- currently only round theme
          theme = "round",
          -- border type can be single,double,rounded,solid,shadow.
          border = "rounded",
          winblend = 0,
          expand = "",
          collapse = "",
          preview = " ",
          code_action = "",
          diagnostic = "🐞",
          incoming = " ",
          outgoing = " ",
          -- colors = {
          --   normal_bg = "#1d1536",
          --   --title background color
          --   title_bg = "#afd700",
          --   red = "#e95678",
          --   magenta = "#b33076",
          --   orange = "#FF8700",
          --   yellow = "#f7bb3b",
          --   green = "#afd700",
          --   cyan = "#36d0e0",
          --   blue = "#61afef",
          --   purple = "#CBA6F7",
          --   white = "#d1d4cf",
          --   black = "#1c1c19",
          -- },
          -- kind = {},
        },
      })
    end,
  },
  {
    "b0o/schemastore.nvim",
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    event = { "BufReadPre" },
    dependencies = { "neovim/nvim-lspconfig" },
  },
  {
    "jose-elias-alvarez/null-ls.nvim",
    event = { "BufReadPre" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    dependencies = { "neovim/nvim-lspconfig", "nvim-lua/plenary.nvim" },
    config = function()
      local data_dir = vim.fn.stdpath("data") .. "/cspell"
      local cspell_dic = {
        dotfiles = {
          name = "dotfiles",
          path = vim.fn.stdpath("config") .. "/cspell/dotfiles.txt",
        },
        user = {
          name = "user",
          path = data_dir .. "/user.txt",
        },
      }
      -- vim辞書がなければダウンロード
      if vim.fn.filereadable(data_dir .. "/vim.txt.gz") ~= 1 then
        local vim_dictionary_url = "https://github.com/iamcco/coc-spell-checker/raw/master/dicts/vim/vim.txt.gz"
        io.popen("curl -fsSLo " .. data_dir .. "/vim.txt.gz --create-dirs " .. vim_dictionary_url)
      end

      -- ユーザー辞書がなければ作成
      if vim.fn.filereadable(cspell_dic.user.path) ~= 1 then
        io.popen("mkdir -p " .. data_dir)
        io.popen("touch " .. cspell_dic.user.path)
      end

      local null_ls = require("null-ls")

      local cspell_code_action = {
        method = null_ls.methods.CODE_ACTION,
        name = "cspell",
        filetypes = {},
        generator = {
          fn = function(params)
            local actions = {}
            local lnum = vim.api.nvim_win_get_cursor(0)[1] - 1
            local diagnostics = vim.diagnostic.get(params.bufnr, { lnum = lnum })
            if vim.tbl_isempty(diagnostics) then
              return
            end

            for _, diagnostic in pairs(diagnostics) do
              if diagnostic.source == "cspell" then
                local msg = diagnostic.message
                local w = msg:match("%b()")
                w = w:sub(2, #w - 1)

                for _, d in pairs(cspell_dic) do
                  local is_exists = false
                  local title = string.format("Add '%s' to %s dictionary", w, d.name)

                  for _, a in pairs(actions) do
                    if a.title == title then
                      is_exists = true
                    end
                  end

                  if is_exists then
                    goto continue
                  end

                  table.insert(actions, {
                    title = title,
                    action = function()
                      local f, err = io.open(d.path, "a+")
                      if not f then
                        vim.notify(err, vim.log.levels.ERROR, { title = "[null-ls] cspell" })
                        return
                      end

                      f:write(w, "\n")
                      f:close()
                      vim.notify(string.format("Added '%s'", w), vim.log.levels.INFO, { title = "[null-ls] cspell" })

                      local q = {
                        name = "cspell",
                        methods = { [null_ls.methods.DIAGNOSTICS] = true },
                        id = params.bufnr,
                      }
                      null_ls.disable(q)
                      null_ls.enable(q)
                    end,
                  })
                  ::continue::
                end
              end
            end

            return actions
          end,
        },
      }
      null_ls.register(cspell_code_action)

      null_ls.setup({
        sources = {
          null_ls.builtins.diagnostics.cspell.with({
            extra_args = { "--config", vim.fn.stdpath("config") .. "/cspell/cspell.json" },
            disabled_filetypes = { "NvimTree" },
            diagnostics_postprocess = function(diagnostic)
              -- レベルをWARNに変更（デフォルトはERROR）
              diagnostic.severity = vim.diagnostic.severity["WARN"]
            end,
            condition = function()
              -- cspellが実行できるときのみ有効
              return vim.fn.executable("cspell") > 0
            end,
            timeout = 50000,
          }),
          null_ls.builtins.code_actions.cspell,
          null_ls.builtins.code_actions.shellcheck,
          null_ls.builtins.diagnostics.flake8.with({
            extra_args = { "--max-line-length", "120" },
          }),
          null_ls.builtins.diagnostics.mypy,
          null_ls.builtins.diagnostics.golangci_lint.with({
            args = {
              "run",
              "--fix=false",
              "--out-format=json",
              "--path-prefix",
              "$ROOT",
            },
            timeout = 50000,
          }),
          null_ls.builtins.formatting.prettier.with({
            timeout = 50000,
          }),
          -- null_ls.builtins.formatting.gofmt,
          null_ls.builtins.formatting.gofumpt,
          null_ls.builtins.formatting.goimports,
          null_ls.builtins.formatting.rustfmt,
          null_ls.builtins.formatting.autopep8,
          null_ls.builtins.formatting.isort,
          null_ls.builtins.formatting.stylua,
          null_ls.builtins.formatting.terraform_fmt,
          null_ls.builtins.formatting.shfmt,
        },
        update_in_insert = false,
        diagnostics_format = "[#{s} #{c}] #{m}",
        debounce = 250,
        default_timeout = 5000,
        debug = false,
        log = {
          enable = true,
          level = "warn",
          use_console = "async",
        },
      })
    end,
  },
  {
    "hrsh7th/nvim-cmp",
    event = { "InsertEnter", "CmdlineEnter" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    dependencies = {
      { "windwp/nvim-autopairs" },
      { "onsails/lspkind.nvim" },
      {
        "L3MON4D3/LuaSnip",
        dependencies = {
          "rafamadriz/friendly-snippets",
          config = function()
            require("luasnip.loaders.from_vscode").lazy_load()
          end,
        },
        opts = {
          history = true,
          delete_check_events = "TextChanged",
        },
        -- stylua: ignore
        keys = {
          {
            "<C-j>",
            function()
              return require("luasnip").jumpable(1) and "<Plug>luasnip-jump-next" or "<tab>"
            end,
            expr = true, silent = true, mode = "i",
          },
          { "<C-j>", function() require("luasnip").jump(1) end, mode = "s" },
          { "<C-k>", function() require("luasnip").jump(-1) end, mode = { "i", "s" } },
        },
      },
      { "saadparwaiz1/cmp_luasnip" },
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-path" },
      { "hrsh7th/cmp-cmdline" },
      {
        "petertriho/cmp-git",
        dependencies = "nvim-lua/plenary.nvim",
        config = function()
          require("cmp_git").setup({
            filetypes = { "NeogitCommitMessage", "gitcommit" },
          })
        end,
      },
      {
        "hrsh7th/cmp-nvim-lsp",
        config = function()
          require("cmp_nvim_lsp").setup()
        end,
      },
      -- { "hrsh7th/cmp-nvim-lsp-signature-help" },
      { "hrsh7th/cmp-nvim-lua" },
      { "windwp/nvim-autopairs" },
    },
    config = function()
      local cmp = require("cmp")
      local types = require("cmp.types")
      local compare = require("cmp.config.compare")
      local lspkind = require("lspkind")

      cmp.setup({
        enabled = function()
          return vim.api.nvim_buf_get_option(0, "buftype") ~= "prompt"
        end,
        view = {
          entries = { name = "custom", selection_order = "top_down" },
        },
        completion = {
          autocomplete = { types.cmp.TriggerEvent.TextChanged },
          keyword_pattern = [[\%(-\?\d\+\%(\.\d\+\)\?\|\h\w*\%(-\w*\)*\)]],
          keyword_length = 1,
        },
        preselect = types.cmp.PreselectMode.None,
        sorting = {
          priority_weight = 2,
          comparators = {
            compare.score,
            compare.recently_used,
            compare.locality,
            compare.kind,
            compare.sort_text,
            compare.length,
            compare.offset,
            compare.exact,
            -- compare.scopes,
            compare.order,
          },
        },
        -- You should change this example to your chosen snippet engine.
        snippet = {
          expand = function(args)
            -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
            require("luasnip").lsp_expand(args.body)
            -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
            -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
          end,
        },

        -- You must set mapping.
        mapping = cmp.mapping.preset.insert({
          ["<C-u>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
          ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
          ["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "s", "c" }),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping({
            i = cmp.mapping.abort(),
            c = cmp.mapping.close(),
          }),
          ["<CR>"] = cmp.mapping.confirm({ select = false }),
        }),

        -- You should specify your *installed* sources.
        sources = cmp.config.sources({
          {
            name = "nvim_lsp",
            priority = 11,
            max_item_count = 50,
          },
          { name = "nvim_lsp_signature_help" },
          {
            -- name = "vsnip",
            name = "luasnip",
            priority = 10,
            max_item_count = 50,
          },
          {
            name = "path",
            max_item_count = 20,
          },
          {
            name = "nvim_lua",
            priority = 12,
            max_item_count = 50,
          },
          {
            name = "buffer",
            priority = 2,
            keyword_length = 2,
            max_item_count = 50,
            option = {
              get_bufnrs = function()
                return vim.api.nvim_list_bufs()
              end,
            },
          },
        }),
        formatting = {
          format = lspkind.cmp_format({
            mode = "symbol_text",
            menu = {
              buffer = "[BUF]",
              nvim_lsp = "[LSP]",
              path = "[PATH]",
              vsnip = "[SNIP]",
              luasnip = "[SNIP]",
              nvim_lua = "[LUA]",
            },
          }),
        },
        experimental = {
          native_menu = false,
          ghost_text = true,
        },
      })

      local cmp_autopairs = require("nvim-autopairs.completion.cmp")
      cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

      require("scripts/cmp/conventionalprefix")
      cmp.setup.filetype({ "NeogitCommitMessage", "gitcommit" }, {
        sources = cmp.config.sources({
          { name = "conventionalprefix" },
          { name = "git" },
        }),
        {
          { name = "buffer" },
        },
      })

      -- Use buffer source for `/`.
      cmp.setup.cmdline("/", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" },
        },
      })

      -- Use cmdline & path source for ':'.
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          { name = "cmdline" },
        }),
      })
    end,
  },
  {
    "hrsh7th/nvim-gtd",
    event = { "BufReadPre" },
    dependencies = { "neovim/nvim-lspconfig" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    config = function()
      require("gtd").setup({})
    end,
  },
  {
    "williamboman/warden.nvim",
    event = { "BufReadPre" },
    enabled = false,
    dependencies = { "neovim/nvim-lspconfig" },
    -- enabled = function()
    --   return vim.g.lsp_client_type == "neovim"
    -- end,
  },
  {
    "folke/trouble.nvim",
    dependencies = { "neovim/nvim-lspconfig" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    cmd = { "Trouble", "TroubleToggle", "TroubleClose", "TroubleRefresh" },
    config = function()
      require("trouble").setup({
        position = "bottom", -- position of the list can be: bottom, top, left, right
        height = 10, -- height of the trouble list when position is top or bottom
        width = 50, -- width of the list when position is left or right
        icons = true, -- use devicons for filenames
        mode = "workspace_diagnostics", -- "workspace_diagnostics", "document_diagnostics", "quickfix", "lsp_references", "loclist"
        fold_open = "", -- icon used for open folds
        fold_closed = "", -- icon used for closed folds
        group = true, -- group results by file
        padding = true, -- add an extra new line on top of the list
        action_keys = { -- key mappings for actions in the trouble list
          -- map to {} to remove a mapping, for example:
          -- close = {},
          close = "q", -- close the list
          cancel = "<esc>", -- cancel the preview and get back to your last window / buffer / cursor
          refresh = "r", -- manually refresh
          jump = { "<cr>", "<tab>" }, -- jump to the diagnostic or open / close folds
          open_split = { "<c-x>" }, -- open buffer in new split
          open_vsplit = { "<c-v>" }, -- open buffer in new vsplit
          open_tab = { "<c-t>" }, -- open buffer in new tab
          jump_close = { "o" }, -- jump to the diagnostic and close the list
          toggle_mode = "m", -- toggle between "workspace" and "document" diagnostics mode
          toggle_preview = "P", -- toggle auto_preview
          hover = "K", -- opens a small popup with the full multiline message
          preview = "p", -- preview the diagnostic location
          close_folds = { "zM", "zm" }, -- close all folds
          open_folds = { "zR", "zr" }, -- open all folds
          toggle_fold = { "zA", "za" }, -- toggle fold of current file
          previous = "k", -- previous item
          next = "j", -- next item
        },
        indent_lines = true, -- add an indent guide below the fold icons
        auto_open = false, -- automatically open the list when you have diagnostics
        auto_close = false, -- automatically close the list when you have no diagnostics
        auto_preview = true, -- automatically preview the location of the diagnostic. <esc> to close preview and go back to last window
        auto_fold = false, -- automatically fold a file trouble list at creation
        auto_jump = { "lsp_definitions" }, -- for the given modes, automatically jump if there is only a single result
        signs = {
          -- icons / text used for a diagnostic
          error = "",
          warning = "",
          hint = "",
          information = "",
          other = "﫠",
        },
        use_diagnostic_signs = false, -- enabling this will use the signs defined in your lsp client
      })
    end,
  },
  {
    "akinsho/flutter-tools.nvim",
    dependencies = { "neovim/nvim-lspconfig" },
    ft = { "dart" },
  },
  {
    "neoclide/coc.nvim",
    event = "VeryLazy",
    enabled = function()
      return vim.g.lsp_client_type == "coc"
    end,
    build = "yarn install --frozen-lockfile",
    config = function()
      local keyset = vim.keymap.set
      vim.g.coc_global_extensions = {
        "coc-word",
        "coc-pairs",
        "coc-lists",
        "coc-diagnostic",
        "coc-json",
        "coc-yaml",
        "coc-marketplace",
        "coc-html",
        "coc-css",
        "coc-tsserver",
        -- "coc-eslint",
        -- "coc-prettier",
        "coc-pyright",
        "coc-rust-analyzer",
        "coc-vimlsp",
        "coc-go",
        -- "coc-lua",
        "coc-sumneko-lua",
        "coc-sql",
        "coc-sh",
        "coc-emoji",
        "coc-gitignore",
        "coc-docker",
        "coc-spell-checker",
        "coc-snippets",
        "https://github.com/cstrap/python-snippets",
      }
      -- Use <C-j> for jump to next placeholder, it's default of coc.nvim
      vim.g.coc_snippet_next = "<c-j>"

      -- Use <C-k> for jump to previous placeholder, it's default of coc.nvim
      vim.g.coc_snippet_prev = "<c-k>"

      -- Autocomplete
      function _G.check_back_space()
        local col = vim.fn.col(".") - 1
        return col == 0 or vim.fn.getline("."):sub(col, col):match("%s") ~= nil
      end

      -- Use Tab for trigger completion with characters ahead and navigate
      -- NOTE: There's always a completion item selected by default, you may want to enable
      -- no select by setting `"suggest.noselect": true` in your configuration file
      -- NOTE: Use command ':verbose imap <tab>' to make sure Tab is not mapped by
      -- other plugins before putting this into your config
      local opts = { silent = true, noremap = true, expr = true, replace_keycodes = false }
      keyset(
        "i",
        "<TAB>",
        'coc#pum#visible() ? coc#pum#next(1) : v:lua.check_back_space() ? "<TAB>" : coc#refresh()',
        opts
      )
      keyset("i", "<S-TAB>", [[coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"]], opts)

      -- Make <CR> to accept selected completion item or notify coc.nvim to format
      -- <C-g>u breaks current undo, please make your own choice
      keyset("i", "<cr>", [[coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"]], opts)

      -- Use <c-j> to trigger snippets
      keyset("i", "<c-j>", "<Plug>(coc-snippets-expand-jump)")
      -- Use <c-space> to trigger completion
      keyset("i", "<c-space>", "coc#refresh()", { silent = true, expr = true })

      -- Use `[g` and `]g` to navigate diagnostics
      -- Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
      keyset("n", "<leander>fp", "<Plug>(coc-diagnostic-prev)", { silent = true })
      keyset("n", "<leader>dn", "<Plug>(coc-diagnostic-next)", { silent = true })

      -- GoTo code navigation
      keyset("n", "gd", "<Plug>(coc-definition)", { silent = true })
      keyset("n", "gy", "<Plug>(coc-type-definition)", { silent = true })
      keyset("n", "gi", "<Plug>(coc-implementation)", { silent = true })
      keyset("n", "gr", "<Plug>(coc-references)", { silent = true })

      -- Use K to show documentation in preview window
      function _G.show_docs()
        local cw = vim.fn.expand("<cword>")
        if vim.fn.index({ "vim", "help" }, vim.bo.filetype) >= 0 then
          vim.api.nvim_command("h " .. cw)
        elseif vim.api.nvim_eval("coc#rpc#ready()") then
          vim.fn.CocActionAsync("doHover")
        else
          vim.api.nvim_command("!" .. vim.o.keywordprg .. " " .. cw)
        end
      end

      keyset("n", "K", "<CMD>lua _G.show_docs()<CR>", { silent = true })

      -- Highlight the symbol and its references on a CursorHold event(cursor is idle)
      vim.api.nvim_create_augroup("CocGroup", {})
      vim.api.nvim_create_autocmd("CursorHold", {
        group = "CocGroup",
        command = "silent call CocActionAsync('highlight')",
        desc = "Highlight symbol under cursor on CursorHold",
      })

      -- Symbol renaming
      keyset("n", "<leader>rn", "<Plug>(coc-rename)", { silent = true })

      -- Formatting selected code
      keyset("x", "<leader>f", "<Plug>(coc-format-selected)", { silent = true })
      keyset("n", "<leader>f", "<Plug>(coc-format-selected)", { silent = true })

      -- Setup formatexpr specified filetype(s)
      vim.api.nvim_create_autocmd("FileType", {
        group = "CocGroup",
        pattern = "typescript,json",
        command = "setl formatexpr=CocAction('formatSelected')",
        desc = "Setup formatexpr specified filetype(s).",
      })

      -- Update signature help on jump placeholder
      vim.api.nvim_create_autocmd("User", {
        group = "CocGroup",
        pattern = "CocJumpPlaceholder",
        command = "call CocActionAsync('showSignatureHelp')",
        desc = "Update signature help on jump placeholder",
      })

      opts = { silent = true, nowait = true }
      -- Apply codeAction to the selected region
      -- Example: `<leader>aap` for current paragraph
      keyset("x", "<leader>a", "<Plug>(coc-codeaction-selected)", opts)
      keyset("n", "<leader>a", "<Plug>(coc-codeaction-selected)", opts)

      -- Remap keys for apply code actions at the cursor position.
      keyset("n", "<leader>ac", "<Plug>(coc-codeaction-cursor)", opts)
      -- Remap keys for apply code actions affect whole buffer.
      keyset("n", "<leader>as", "<Plug>(coc-codeaction-source)", opts)
      -- Remap keys for applying codeActions to the current buffer
      -- keyset("n", "<leader>ac", "<Plug>(coc-codeaction)", opts)
      -- Apply the most preferred quickfix action on the current line.
      keyset("n", "<leader>qf", "<Plug>(coc-fix-current)", opts)

      -- Remap keys for apply refactor code actions.
      keyset("n", "<leader>re", "<Plug>(coc-codeaction-refactor)", { silent = true })
      keyset("x", "<leader>r", "<Plug>(coc-codeaction-refactor-selected)", { silent = true })
      keyset("n", "<leader>r", "<Plug>(coc-codeaction-refactor-selected)", { silent = true })

      -- Run the Code Lens actions on the current line
      keyset("n", "<leader>cl", "<Plug>(coc-codelens-action)", opts)

      -- Map function and class text objects
      -- NOTE: Requires 'textDocument.documentSymbol' support from the language server
      keyset("x", "if", "<Plug>(coc-funcobj-i)", opts)
      keyset("o", "if", "<Plug>(coc-funcobj-i)", opts)
      keyset("x", "af", "<Plug>(coc-funcobj-a)", opts)
      keyset("o", "af", "<Plug>(coc-funcobj-a)", opts)
      keyset("x", "ic", "<Plug>(coc-classobj-i)", opts)
      keyset("o", "ic", "<Plug>(coc-classobj-i)", opts)
      keyset("x", "ac", "<Plug>(coc-classobj-a)", opts)
      keyset("o", "ac", "<Plug>(coc-classobj-a)", opts)

      -- Remap <C-f> and <C-b> to scroll float windows/popups
      ---@diagnostic disable-next-line: redefined-local
      local opts = { silent = true, nowait = true, expr = true }
      keyset("n", "<C-d>", 'coc#float#has_scroll() ? coc#float#scroll(1) : "<C-f>"', opts)
      keyset("n", "<C-u>", 'coc#float#has_scroll() ? coc#float#scroll(0) : "<C-b>"', opts)
      keyset("i", "<C-d>", 'coc#float#has_scroll() ? "<c-r>=coc#float#scroll(1)<cr>" : "<Right>"', opts)
      keyset("i", "<C-u>", 'coc#float#has_scroll() ? "<c-r>=coc#float#scroll(0)<cr>" : "<Left>"', opts)
      keyset("v", "<C-d>", 'coc#float#has_scroll() ? coc#float#scroll(1) : "<C-f>"', opts)
      keyset("v", "<C-u>", 'coc#float#has_scroll() ? coc#float#scroll(0) : "<C-b>"', opts)

      -- Use CTRL-S for selections ranges
      -- Requires 'textDocument/selectionRange' support of language server
      keyset("n", "<C-s>", "<Plug>(coc-range-select)", { silent = true })
      keyset("x", "<C-s>", "<Plug>(coc-range-select)", { silent = true })

      -- Add `:Format` command to format current buffer
      vim.api.nvim_create_user_command("Format", "call CocAction('format')", {})

      -- " Add `:Fold` command to fold current buffer
      vim.api.nvim_create_user_command("Fold", "call CocAction('fold', <f-args>)", { nargs = "?" })

      -- Add `:OR` command for organize imports of the current buffer
      vim.api.nvim_create_user_command("OR", "call CocActionAsync('runCommand', 'editor.action.organizeImport')", {})

      keyset("n", "<leader>F", ":Format<CR>", { silent = true })
      keyset("n", "<leader>O", ":OR<CR>", { silent = true })

      -- Add (Neo)Vim's native statusline support
      -- NOTE: Please see `:h coc-status` for integrations with external plugins that
      -- provide custom statusline: lightline.vim, vim-airline
      vim.opt.statusline:prepend("%{coc#status()}%{get(b:,'coc_current_function','')}")
    end,
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    config = function()
      local wk = require("which-key")
      wk.setup({
        plugins = {
          marks = true, -- shows a list of your marks on ' and `
          registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
          spelling = {
            enabled = false, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
            suggestions = 20, -- how many suggestions should be shown in the list?
          },
          -- the presets plugin, adds help for a bunch of default keybindings in Neovim
          -- No actual key bindings are created
          presets = {
            operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
            motions = true, -- adds help for motions
            text_objects = true, -- help for text objects triggered after entering an operator
            windows = true, -- default bindings on <c-w>
            nav = true, -- misc bindings to work with windows
            z = true, -- bindings for folds, spelling and others prefixed with z
            g = true, -- bindings for prefixed with g
          },
        },
        -- add operators that will trigger motion and text object completion
        -- to enable all native operators, set the preset / operators plugin above
        operators = { gc = "Comments" },
        key_labels = {
          -- override the label used to display some keys. It doesn't effect WK in any other way.
          -- For example:
          ["<space>"] = "SPC",
          ["<cr>"] = "RET",
          ["<tab>"] = "TAB",
        },
        icons = {
          breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
          separator = "➜", -- symbol used between a key and it's label
          group = "+", -- symbol prepended to a group
        },
        popup_mappings = {
          scroll_down = "<c-d>", -- binding to scroll down inside the popup
          scroll_up = "<c-u>", -- binding to scroll up inside the popup
        },
        window = {
          border = "shadow", -- none, single, double, shadow
          position = "bottom", -- bottom, top
          margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
          padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
          winblend = 10,
        },
        layout = {
          height = { min = 4, max = 25 }, -- min and max height of the columns
          width = { min = 20, max = 50 }, -- min and max width of the columns
          spacing = 3, -- spacing between columns
          align = "left", -- align columns left, center or right
        },
        ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
        hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
        show_help = true, -- show help message on the command line when the popup is visible
        triggers = "auto", -- automatically setup triggers
        -- triggers = {"<leader>"} -- or specify a list manually
        triggers_blacklist = {
          -- list of mode / prefixes that should never be hooked by WhichKey
          -- this is mostly relevant for key maps that start with a native binding
          -- most people should not need to change this
          i = { "j", "k" },
          v = { "j", "k" },
        },
      })

      -- mode: n
      wk.register({
        ["R"] = { "<Plug>(operator-replace)", "Replace" },
        -- ["<leader>t"] = {
        --   name = "+Test",
        --   r = {
        --     name = "+Run",
        --     n = { '<cmd>lua require("neotest").run.run()<CR>', "Nearest" },
        --     f = { '<cmd>lua require("neotest").run.run(vim.fn.expand("%"))<CR>', "Current File" },
        --     r = { '<cmd>lua require("neotest").run.run_last()<CR>', "Last" },
        --     s = { '<cmd>lua require("neotest").run.stop()<CR>', "Stop" },
        --     a = { '<cmd>lua require("neotest").run.attach()<CR>', "Attach" },
        --   },
        --   o = {
        --     name = "+Display",
        --     s = { '<cmd>lua require("neotest").summary.toggle()<CR>', "Summary" },
        --     o = { '<cmd>lua require("neotest").output.open({enter = true})<CR>', "Output Enter" },
        --     O = { '<cmd>lua require("neotest").output.open()<CR>', "Output" },
        --   },
        -- },
        ["<leader>t"] = {
          name = "+Test",
          r = {
            name = "+Run",
            n = { "<cmd>TestNearest<cr>", "Nearest" },
            f = { "<cmd>TestFile<cr>", "Current File" },
            s = { "<cmd>TestSuite<cr>", "Suit" },
            r = { "<cmd>TestLast<cr>", "Last" },
          },
        },
        ["<leader>u"] = {
          name = "+Utility",
          c = {
            name = "+Color",
            p = { "<cmd>CccPick<CR>", "Picker" },
          },
        },
        ["<leader>w"] = {
          name = "+Window",
          w = { "<cmd>Chowcho<cr>", "Selector" },
          r = { "<cmd>WinResizerStartResize<cr>", "Resize" },
        },
        ["<leader>s"] = {
          name = "+Search",
          j = {
            "<cmd>AnyJump<CR>",
            "AnyJump",
          },
          b = {
            '<cmd>lua require("telescope.builtin").buffers{ show_all_buffers = true, generic_sorters = require("telescope.sorters").fuzzy_with_index_bias }<CR>',
            "Buffer",
          },
          m = {
            '<cmd>lua require("telescope.builtin").keymaps{ }<CR>',
            "Keymaps",
          },
          c = {
            name = "+Commands",
            r = { "<cmd>lua require('telescope.builtin').command_history{}<CR>", "History" },
            c = { "<cmd>lua require('telescope.builtin').commands{}<CR>", "Commands" },
          },
          f = {
            name = "+Files",
            f = {
              '<cmd>lua require("telescope.builtin").find_files{ find_command = {"rg", "-i", "--hidden", "--files", "-g", "!.git"} }<CR>',
              "Find Files",
            },
            g = { "<cmd>lua require('telescope.builtin').git_files{}<CR>", "Git Files" },
            j = { "<cmd>lua require('telescope.builtin').jumplist{}<CR>", "Jump List" },
            l = { "<cmd>lua require('telescope.builtin').loclist{}<CR>", "Location list" },
            r = { "<cmd>lua require('telescope.builtin').oldfiles{cwd_only = true}<CR>", "Old Files" },
            q = { "<cmd>lua require('telescope.builtin').quickfix{}<CR>", "Quickfix" },
          },
          v = {
            name = "+Git",
            c = { "<cmd>lua require('telescope.builtin').git_bcommits{}<CR>", "Buffer Commit" },
            C = { "<cmd>lua require('telescope.builtin').git_commits{}<CR>", "Commit" },
            s = { "<cmd>lua require('telescope.builtin').git_status{}<CR>", "Status" },
            b = { "<cmd>lua require('telescope.builtin').git_branches{}<CR>", "Branch" },
          },
          g = {
            name = "+Grep",
            g = {
              '<cmd>lua require("telescope.builtin").live_grep{ glob_pattern = "!.git" }<CR>',
              "Live Grep",
            },
            c = { "<cmd>lua require('telescope.builtin').grep_string{}<CR>", "Grep String" },
          },
          r = { "<cmd>lua require('telescope.builtin').resume{}<CR>", "Resume" },
          t = { "<cmd>AerialToggle<CR>", "ToC" },
          T = { "<cmd>AerialToggle!<CR>", "ToC" },
        },
        ["<leader>g"] = {
          name = "+Git",
          s = { "<cmd>lua require('neogit').open({ kind = 'vsplit' })<cr>", "Status" },
          d = {
            name = "+Diff",
            d = { "<cmd>Gitsigns diffthis<cr>", "Diff" },
          },
          l = {
            name = "+Linker",
            c = { "<cmd>GitLinker current<cr>", "Current git link" },
            d = { "<cmd>GitLinker default<cr>", "Default branch git link" },
          },
        },
        ["<leader>c"] = {
          name = "+Comment",
          g = {
            name = "+Generate",
            f = { '<cmd>lua require("neogen").generate { type = "func" }<CR>', "Generate doc comment for function" },
            F = { '<cmd>lua require("neogen").generate { type = "file" }<CR>', "Generate doc comment for file" },
            t = { '<cmd>lua require("neogen").generate { type = "type" }<CR>', "Generate doc comment for type" },
            c = { '<cmd>lua require("neogen").generate { type = "class" }<CR>', "Generate doc comment for class" },
          },
        },
        ["m"] = {
          name = "+Move",
          n = {
            name = "+Next",
            s = {
              name = "+Start",
              m = { "<cmd>TSTextobjectGotoNextStart @function.outer<cr>", "Method" },
              b = { "<cmd>TSTextobjectGotoNextStart @block.outer<cr>", "Block" },
            },
            e = {
              name = "+End",
              m = { "<cmd>TSTextobjectGotoNextEnd @function.outer<cr>", "Method" },
              b = { "<cmd>TSTextobjectGotoNextEnd @block.outer<cr>", "Block" },
            },
          },
          p = {
            name = "+Previous",
            s = {
              name = "+Start",
              m = { "<cmd>TSTextobjectGotoPreviousStart @function.outer<cr>", "Method" },
              b = { "<cmd>TSTextobjectGotoPreviousStart @block.outer<cr>", "Block" },
            },
            e = {
              name = "+End",
              m = { "<cmd>TSTextobjectGotoPreviousEnd @function.outer<cr>", "Method" },
              b = { "<cmd>TSTextobjectGotoPreviousEnd @block.outer<cr>", "Block" },
            },
          },
        },
        ["v"] = {
          name = "+Select",
          i = {
            name = "+Inner",
            m = { "<cmd>TSTextobjectSelect @function.inner<cr>", "Select Method" },
            b = { "<cmd>TSTextobjectSelect @block.inner<cr>", "Select Block" },
            p = { "<cmd>TSTextobjectSelect @parameter.inner<cr>", "Select Parameter" },
          },
          o = {
            name = "+Outer",
            m = { "<cmd>TSTextobjectSelect @function.outer<cr>", "Select Method" },
            b = { "<cmd>TSTextobjectSelect @block.outer<cr>", "Select Block" },
            p = { "<cmd>TSTextobjectSelect @parameter.outer<cr>", "Select Parameter" },
          },
        },
      })

      -- mode: x
      wk.register({
        ["<leader>g"] = {
          name = "+Git",
          s = {
            name = "+Stage",
            s = { ":Gitsigns stage_hunk<cr>", "Select Stage" },
            u = { ":Gitsigns unstage_hunk<cr>", "Select Unstage" },
          },
          l = {
            name = "+Linker",
            c = { ":GitLinker current<cr>", "Current git link" },
            d = { ":GitLinker default<cr>", "Default branch git link" },
          },
        },
      }, { mode = "x" })

      if vim.g.file_explorer_type == "nvim-tree" then
        wk.register({
          ["<leader>f"] = {
            name = "+Explorer",
            t = { "<cmd>NvimTreeToggle<cr>", "Toggle" },
            f = {
              '<cmd>lua require("nvim-tree.api").tree.find_file{ open=true, update_root = false, focus = true }<cr>',
              "Focus File",
            },
          },
        })
      elseif vim.g.file_explorer_type == "neo-tree" then
        wk.register({
          ["<leader>f"] = {
            name = "+Explorer",
            t = { "<cmd>Neotree toggle<cr>", "Toggle" },
            f = {
              "<cmd>Neotree reveal<cr>",
              "Focus File",
            },
          },
        })
      end

      if vim.g.lsp_client_type == "neovim" then
        wk.register({
          ["g"] = {
            name = "+LSP",
            -- r = { "<cmd>Lspsaga lsp_finder<CR>", "References" },
            -- d = { "<cmd>Lspsaga peek_definition<CR>", "Definition" },
            r = { "<cmd>Telescope lsp_references<CR>", "References" },
            i = { "<cmd>Telescope lsp_implementations<CR>", "Implementations" },
            d = { "<cmd>Telescope lsp_definitions<CR>", "Definition" },
            D = { "<cmd>Telescope lsp_type_definitions<CR>", "Type Definition" },
            -- c = {
            --   name = "+Callhierarchy",
            --   i = { "<cmd>Lspsaga incoming_calls<CR>", "Incoming" },
            --   o = { "<cmd>Lspsaga outgoing_calls<CR>", "Outgoing" },
            -- },
            f = {
              name = "+Go To Definition",
              f = { "<cmd>lua require('gtd').exec({ command = 'edit' })<CR>", "Go to edit" },
              s = { "<cmd>lua require('gtd').exec({ command = 'split' })<CR>", "Go to split" },
              v = { "<cmd>lua require('gtd').exec({ command = 'vsplit' })<CR>", "Go to vsplit" },
            },
          },
          -- ["K"] = {
          --   "<cmd>Lspsaga hover_doc<CR>",
          --   "Hover Doc",
          -- },
          ["K"] = {
            function()
              local buf = vim.api.nvim_get_current_buf()
              local ft = vim.api.nvim_buf_get_option(buf, "filetype")
              if ft == "rust" then
                return require("rust-tools").hover_actions.hover_actions()
              end
              return vim.lsp.buf.hover()
            end,
            "Hover Doc",
          },
          ["H"] = { "<cmd>lua vim.lsp.buf.signature_help()<CR>", "Signature Help" },
          ["<leader>"] = {
            F = { "<cmd>lua require('scripts/util').lsp.null_ls_formatting()<CR>", "Format" },
          },
          -- ["<leader>ac"] = { "<cmd>Lspsaga code_action<CR>", "Code Action" },
          ["<leader>ac"] = { "<cmd>lua vim.lsp.buf.code_action()<CR>", "Code Action" },
          ["<leader>d"] = {
            name = "+Diagnostics",
            -- c = { "<cmd>Lspsaga show_cursor_diagnostics<CR>", "Current cursor" },
            -- n = { "<cmd>Lspsaga diagnostic_jump_next<CR>", "Jump Next" },
            -- p = { "<cmd>Lspsaga diagnostic_jump_prev<CR>", "Jump Previous" },
            -- l = { "<cmd>Lspsaga show_line_diagnostics<CR>", "Current line" },
            -- b = { "<cmd>Lspsaga show_buf_diagnostics<CR>", "Current buffer" },
            c = { "<cmd>lua vim.diagnostic.open_float()<CR>", "Open Float" },
            o = { "<cmd>lua vim.diagnostic.setloclist()<CR>", "Set Loclist" },
            n = { "<cmd>lua vim.diagnostic.goto_next()<CR>", "Jump Next" },
            p = { "<cmd>lua vim.diagnostic.goto_prev()<CR>", "Jump Previous" },
            d = {
              name = "+Diagnostics",
              d = { "<cmd>TroubleToggle document_diagnostics<cr>", "Document Diagnostics(Trouble)" },
              D = { "<cmd>TroubleToggle workspace_diagnostics<cr>", "Workspace Diagnostics(Trouble)" },
            },
          },
          -- ["<leader>rn"] = { "<cmd>Lspsaga rename<CR>", "Rename" },
          ["<leader>rn"] = { "<cmd>lua vim.lsp.buf.rename()<CR>", "Rename" },
        }, { mode = "n" })
        wk.register({
          -- ["<leader>ac"] = {
          --   "<cmd>Lspsaga code_action<CR>",
          --   "Code Action",
          -- },
          ["<leader>ac"] = {
            "<cmd>lua vim.lsp.buf.code_action()<CR>",
            "Code Action",
          },
        }, { mode = "v" })
      elseif vim.g.lsp_client_type == "coc" then
        wk.register({
          ["<lesder>sd"] = { "<cmd>Telescope coc diagnostics<CR>", "Diagnostics" },
          ["<lesder>sD"] = { "<cmd>Telescope coc workspace_diagnostics<CR>", "Workspace Diagnostics" },
          ["<lesder>ca"] = { "<cmd>Telescope coc code_actions<CR>", "Code Actions" },
          ["g"] = {
            name = "+LSP",
            r = { "<cmd>Telescope coc references<CR>", "References" },
            i = { "<cmd>Telescope coc implementations<CR>", "Implementations" },
            y = { "<cmd>Telescope coc type_definitions<CR>", "Type Definitions" },
          },
        })
      end
    end,
  },

  -- ColorScheme
  {
    "Everblush/nvim",
    name = "everblush",
    lazy = true,
    config = function()
      require("everblush").setup({
        transparent_background = true,
        nvim_tree = {
          contrast = false,
        },
      })
    end,
  },
  {
    "lmburns/kimbox",
    lazy = true,
    config = function()
      require("kimbox").setup({
        -- options
        -- Main options --
        style = "ocean", -- choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
        -- medium: #231A0C
        -- ocean: #221A02
        -- medium: #231A0C
        -- deep: #0f111B
        -- darker:#291804
        -- General formatting --
        allow_bold = true,
        allow_italic = true,
        allow_underline = true,
        allow_undercurl = true,
        allow_reverse = false,

        transparent = false, -- don't set background
        term_colors = true, -- if true enable the terminal
        ending_tildes = false, -- show the end-of-buffer tildes
      })
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    lazy = true,
    config = function()
      require("kanagawa").setup({
        undercurl = true, -- enable undercurls
        commentStyle = { italic = true },
        keywordStyle = { italic = true },
        statementStyle = { bold = true },
        variablebuiltinStyle = { italic = true },
        specialReturn = true, -- special highlight for the return keyword
        specialException = true, -- special highlight for exception handling keywords
        transparent = true, -- do not set background color
        dimInactive = false, -- dim inactive window `:h hl-NormalNC`
        globalStatus = true, -- adjust window separators highlight for laststatus=3
      })
    end,
  },
  {
    "rmehri01/onenord.nvim",
    lazy = true,
    config = function()
      require("onenord").setup({
        theme = "dark", -- "dark" or "light". Alternatively, remove the option and set vim.o.background instead
        borders = true, -- Split window borders
        fade_nc = false, -- Fade non-current windows, making them more distinguishable
        styles = {
          comments = "italic", -- Style that is applied to comments: see `highlight-args` for options
          strings = "NONE", -- Style that is applied to strings: see `highlight-args` for options
          keywords = "bold", -- Style that is applied to keywords: see `highlight-args` for options
          functions = "italic", -- Style that is applied to functions: see `highlight-args` for options
          variables = "NONE", -- Style that is applied to variables: see `highlight-args` for options
          diagnostics = "undercurl", -- Style that is applied to diagnostics: see `highlight-args` for options
        },
        disable = {
          background = true, -- Disable setting the background color
          cursorline = false, -- Disable the cursorline
          eob_lines = true, -- Hide the end-of-buffer lines
        },
      })
    end,
  },
  {
    "sainnhe/edge",
    lazy = true,
    config = function()
      vim.g.edge_style = "aura"
      vim.g.edge_enable_italic = true
      vim.g.edge_disable_italic_comment = false
      vim.g.edge_current_word = "bold"
      vim.g.edge_transparent_background = true
    end,
  },
  {
    "sainnhe/everforest",
    lazy = true,
    config = function()
      -- Set contrast.
      --   This configuration option should be placed before `colorscheme everforest`.
      --   Available values: 'hard', 'medium'(default), 'soft'
      vim.g.everforest_background = "soft"
      vim.g.everforest_enable_italic = true
      vim.g.everforest_disable_italic_comment = false
      vim.g.everforest_transparent_background = true
      vim.g.everforest_ui_contrast = "low" -- high or low
      vim.g.everforest_diagnostic_text_highlight = true
      vim.g.everforest_diagnostic_line_highlight = true
    end,
  },
  {
    "folke/tokyonight.nvim",
    lazy = true,
    config = function()
      require("tokyonight").setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        style = "storm", -- The theme comes in three styles, `storm`, a darker variant `night` and `day`
        transparent = true, -- Enable this to disable setting the background color
        terminal_colors = true, -- Configure the colors used when opening a `:terminal` in Neovim
        styles = {
          -- Style to be applied to different syntax groups
          -- Value is any valid attr-list value `:help attr-list`
          comments = "italic",
          keywords = "italic",
          functions = "NONE",
          variables = "NONE",
          -- Background styles. Can be "dark", "transparent" or "normal"
          sidebars = "dark", -- style for sidebars, see below
          floats = "dark", -- style for floating windows
        },
        sidebars = { "qf", "help", "NvimTree" }, -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`
        day_brightness = 0.3, -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors
        hide_inactive_statusline = false, -- Enabling this option, will hide inactive statuslines and replace them with a thin border instead. Should work with the standard **StatusLine** and **LuaLine**.
        dim_inactive = false, -- dims inactive windows
        lualine_bold = false, -- When `true`, section headers in the lualine theme will be bold
      })
    end,
  },
  {
    "eddyekofo94/gruvbox-flat.nvim",
    lazy = true,
    config = function()
      vim.g.gruvbox_flat_style = "dark"
      vim.g.gruvbox_italic_functions = true
      vim.g.gruvbox_italic_comments = true
      vim.g.gruvbox_italic_keywords = true
      vim.g.gruvbox_italic_variables = false
      vim.g.gruvbox_transparent = true
      vim.g.gruvbox_dark_sidebar = true
      vim.g.gruvbox_dark_float = true
      vim.g.gruvbox_sidebars = { "qf", "vista_kind", "terminal", "packer" }
      vim.g.gruvbox_hide_inactive_statusline = true
    end,
  },
  {
    "EdenEast/nightfox.nvim",
    lazy = true,
    config = function()
      local nightfox = require("nightfox")
      nightfox.setup({
        options = {
          transparent = true,
          styles = {
            comments = "italic", -- change style of comments to be italic
            keywords = "bold", -- change style of keywords to be bold
          },
          inverse = {
            match_paren = true, -- inverse the highlighting of match_parens
            visual = false,
            search = false,
          },
        },
      })
      -- nightfox.load()
    end,
  },

  -- Local plugins
  {
    -- "tkmpypy/chowcho.nvim",
    dir = "~/ghq/github.com/tkmpypy/chowcho.nvim",
    config = function()
      require("chowcho").setup({
        border_style = "rounded",
        icon_enabled = true,
        use_default_exclude = true,
        exclude = function(buf, win)
          -- exclude noice.nvim's cmdline_popup
          local bt = vim.api.nvim_buf_get_option(buf, "buftype")
          local ft = vim.api.nvim_buf_get_option(buf, "filetype")
          if bt == "nofile" and (ft == "noice" or ft == "vim") then
            return true
          end
          return false
        end,
      })
    end,
  },
  {
    dir = "~/ghq/github.com/tkmpypy/deepon.nvim",
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("deepon").setup()
    end,
  },
}, {
  defaults = {
    lazy = false, -- should plugins be lazy-loaded?
  },
  concurrency = 50,
  ui = {
    border = "rounded",
  },
  checker = {
    enabled = true,
    concurrency = 50,
  },
  performance = {
    cache = {
      enabled = true,
      -- disable_events = {},
    },
    rtp = {
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "netrw",
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
        "nvim-treesitter-textobjects",
      },
    },
  },
})
