-- bootstraping for lazy.nvim
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "--single-branch",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  }
end
vim.opt.runtimepath:prepend(lazypath)

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
      require("nvim-treesitter.install").update { with_sync = true }
    end,
    config = function()
      require("nvim-treesitter.configs").setup {
        highlight = {
          enable = true,
          disable = function(lang, buf)
            if lang == "typescript" then
              return true
            end
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
          "regex",
          "make",
          "dockerfile",
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
        autotag = {
          enable = true,
        },
        rainbow = {
          enable = true,
          extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
        },
        context_commentstring = { enable = true, enable_autocmd = false },
      }
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
      local config = require "kommentary.config"
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
      require("neogen").setup {
        enabled = true,
        languages = {
          python = {
            template = {
              annotation_convention = "google_docstrings",
            },
          },
        },
      }
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
      vim.cmd [[autocmd BufRead,BufNewFile */templates/*.yml,*/templates/*.yaml,*/templates/*.tpl set ft=helm]]
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
      vim.cmd [[
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
      ]]
    end,
  },
  {
    "thinca/vim-quickrun",
    cmd = "QuickRun",
    config = function()
      vim.cmd [[
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
      ]]
    end,
  },

  -- UI
  "kyazdani42/nvim-web-devicons",
  {
    "rcarriga/nvim-notify",
    config = function()
      require("notify").setup {
        background_colour = "#000000",
      }
      -- vim.notify = require "notify"
      -- vim.notify.setup {
      --   -- Animation style (see below for details)
      --   stages = "fade_in_slide_out",

      --   -- Default timeout for notifications
      --   timeout = 5000,

      --   -- For stages that change opacity this is treated as the highlight behind the window
      --   background_colour = "#000000",

      --   -- Icons for the different levels
      --   icons = {
      --     ERROR = "",
      --     WARN = "",
      --     INFO = "",
      --     DEBUG = "",
      --     TRACE = "✎",
      --   },
      -- }
    end,
  },
  {
    "nvim-zh/colorful-winsep.nvim",
    event = "ColorScheme",
    config = function()
      require("colorful-winsep").setup {
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
      }
    end,
  },
  {
    "levouh/tint.nvim",
    event = "BufReadPre",
    config = function()
      local ignore_ft = { "aerial" }
      require("tint").setup {
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
      }
    end,
  },
  {
    "petertriho/nvim-scrollbar",
    dependencies = { "kevinhwang91/nvim-hlslens" },
    event = { "BufReadPre" },
    config = function()
      require("hlslens").setup()
      require("scrollbar").setup {
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
      }
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
      require("indent_blankline").setup {
        -- char = "",
        enabled = true,
        buftype_exclude = { "terminal", "help", "nofile" },
        filetype_exclude = { "startify", "alpha", "NvimTree" },
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
      }
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    event = { "ColorScheme" },
    dependencies = { "kyazdani42/nvim-web-devicons" },
    config = function()
      local util = require "scripts/util"
      local lualine_utils = require "lualine.utils.utils"
      local theme = require "lualine/themes/onedark"
      require("lualine").setup {
        options = {
          icons_enabled = true,
          theme = "auto",
          -- component_separators = { left = "", right = "" },
          -- section_separators = { left = "", right = "" },
          component_separators = "",
          section_separators = "",
          disabled_filetypes = {},
          always_divide_middle = true,
          globalstatus = true,
        },
        sections = {
          lualine_a = { "mode" },
          lualine_b = {},
          lualine_y = {},
          lualine_z = {},
          lualine_c = {
            {
              "filetype",
              icon_only = true,
            },
            {
              "filename",
              padding = 0,
              symbols = { modified = "  ", readonly = "  ", unnamed = "  " },
              separator = "    ",
              path = 1,
            },
            {
              util.lsp.current_lsp,
              icon = "",
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
                  fg = lualine_utils.extract_color_from_hllist(
                    "fg",
                    { "DiagnosticHint", "DiagnosticSignHint", "LspDiagnosticsDefaultHint", "DiffChange", "HintText" },
                    "#273faf"
                  ),
                },
              },
            },
          },
          lualine_x = {
            {
              "branch",
              color = {
                fg = theme.visual.a.bg,
              },
            },
            {
              "diff",
              -- Is it me or the symbol for modified us really weird
              symbols = { added = " ", modified = "柳", removed = " " },
            },
            "encoding",
            util.buffer.scroll_bar,
          },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { "filename" },
          lualine_x = { "location" },
          lualine_y = {},
          lualine_z = {},
        },
        tabline = {},
        extensions = {
          "fzf",
          "quickfix",
          "toggleterm",
          "symbols-outline",
          "nvim-tree",
        },
      }
    end,
  },
  {
    "goolord/alpha-nvim",
    lazy = false,
    dependencies = { "kyazdani42/nvim-web-devicons" },
    config = function()
      local alpha = require "alpha"
      local theme = require "alpha.themes.theta"
      local dashboard = require "alpha.themes.dashboard"

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
      vim.cmd [[
            autocmd FileType alpha setlocal nofoldenable
        ]]
    end,
  },
  {
    "stevearc/aerial.nvim",
    cmd = "AerialToggle",
    config = function()
      require("aerial").setup {
        attach_mode = "global", -- "window" or "global"
        layout = {
          placement = "window", -- "window" or "edge"
        },
      }
    end,
  },
  {
    "folke/noice.nvim",
    dependencies = {
      "MunifTanjim/nui.nvim",
      "rcarriga/nvim-notify",
    },
    config = function()
      require("noice").setup {
        presets = {
          bottom_search = false, -- use a classic bottom cmdline for search
          command_palette = false, -- position the cmdline and popupmenu together
          long_message_to_split = true, -- long messages will be sent to a split
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
          backend = "nui", -- backend to use to show regular cmdline completions
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
            enabled = true,
          },
          override = {
            -- override the default lsp markdown formatter with Noice
            ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            -- override the lsp markdown formatter with Noice
            ["vim.lsp.util.stylize_markdown"] = true,
            -- override cmp documentation with Noice (needs the other options to work)
            ["cmp.entry.get_documentation"] = true,
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
      }
      -- vim.keymap.set("n", "<c-f>", function()
      --   if not require("noice.lsp").scroll(4) then
      --     return "<c-f>"
      --   end
      -- end, { silent = true, expr = true })

      -- vim.keymap.set("n", "<c-b>", function()
      --   if not require("noice.lsp").scroll(-4) then
      --     return "<c-b>"
      --   end
      -- end, { silent = true, expr = true })
    end,
  },

  -- explorer
  {
    "lambdalisue/fern.vim",
    enabled = false,
    config = function()
      local gid = vim.api.nvim_create_augroup("tkmpypy_fern_settings", { clear = true })
      vim.api.nvim_create_autocmd({ "FileType" }, {
        group = gid,
        pattern = "fern",
        callback = function()
          vim.cmd [[
              setlocal nonumber
              setlocal norelativenumber
              let b:indentLine_enabled = 0

              " Define NERDTree like mappings
              nmap <buffer> o <Plug>(fern-action-open:edit)
              nmap <buffer> go <Plug>(fern-action-open:edit)<C-w>p
              nmap <buffer> t <Plug>(fern-action-open:tabedit)
              nmap <buffer> T <Plug>(fern-action-open:tabedit)gT
              nmap <buffer> <C-x> <Plug>(fern-action-open:split)
              " nmap <buffer> <C-x> <Plug>(fern-action-open:split)<C-w>p
              nmap <buffer> <C-v> <Plug>(fern-action-open:vsplit)
              " nmap <buffer> <C-v> <Plug>(fern-action-open:vsplit)<C-w>p

              nmap <buffer> P gg

              nmap <buffer> C <Plug>(fern-action-enter)
              nmap <buffer> u <Plug>(fern-action-leave)
              nmap <buffer> r <Plug>(fern-action-reload)
              nmap <buffer> R gg<Plug>(fern-action-reload)<C-o>
              nmap <buffer> cd <Plug>(fern-action-cd)
              nmap <buffer> CD gg<Plug>(fern-action-cd)<C-o>

              nmap <buffer> I <Plug>(fern-action-hide-toggle)

              nmap <buffer> q :<C-u>quit<CR>
              nmap <buffer><expr>
                \ <Plug>(fern-my-expand-or-collapse)
                \ fern#smart#leaf(
                \   "\<Plug>(fern-action-collapse)",
                \   "\<Plug>(fern-action-expand)",
                \   "\<Plug>(fern-action-collapse)",
                \ )
              nmap <buffer><expr>
                \ <Plug>(fern-my-expand-or-enter)
                \ fern#smart#drawer(
                \   "\<Plug>(fern-action-open-or-expand)",
                \   "\<Plug>(fern-action-open-or-enter)",
                \ )
              nmap <buffer><expr>
                \ <Plug>(fern-my-collapse-or-leave)
                \ fern#smart#drawer(
                \   "\<Plug>(fern-action-collapse)",
                \   "\<Plug>(fern-action-leave)",
                \ )
              nmap <buffer><nowait> <CR> <Plug>(fern-my-expand-or-enter)
              nmap <buffer><nowait> l <Plug>(fern-my-expand-or-enter)
              nmap <buffer><nowait> h <Plug>(fern-my-collapse-or-leave)

              call glyph_palette#apply()
            ]]
        end,
      })
      vim.keymap.set("n", "<Leader>ft", "<cmd>Fern . -drawer -toggle<CR>", {})
      vim.keymap.set("n", "<Leader>ff", "<cmd>Fern . -reveal=% -drawer -toggle<CR>", {})
      vim.cmd [[
          let g:fern#default_hidden = 1
          let g:fern#drawer_keep = v:false
          let g:fern#keepalt_on_edit = 1
          let g:fern#disable_drawer_tabpage_isolation = 0
          let g:fern#disable_drawer_auto_winfixwidth = 0
          let g:fern#disable_drawer_auto_resize = 0
        ]]
    end,
    requires = {
      { "antoinemadec/FixCursorHold.nvim" },
      {
        "lambdalisue/fern-git-status.vim",
        config = function()
          vim.cmd [[
              " Disable listing ignored files/directories
              let g:fern_git_status#disable_ignored = 0
              " Disable listing untracked files
              let g:fern_git_status#disable_untracked = 0
              " Disable listing status of submodules
              let g:fern_git_status#disable_submodules = 0
              " Disable listing status of directories
              let g:fern_git_status#disable_directories = 0
            ]]
        end,
      },
      {
        "lambdalisue/fern-renderer-nerdfont.vim",
        requires = {
          "lambdalisue/nerdfont.vim",
          "lambdalisue/glyph-palette.vim",
        },
        config = function()
          vim.cmd [[
              let g:fern#renderer = "nerdfont"
            ]]
        end,
      },
      {
        "yuki-yano/fern-preview.vim",
        config = function()
          local gid = vim.api.nvim_create_augroup("tkmpypy_fern_preview_settings", { clear = true })
          vim.api.nvim_create_autocmd({ "FileType" }, {
            group = gid,
            pattern = "fern",
            callback = function()
              vim.cmd [[
                  nmap <silent> <buffer> p     <Plug>(fern-action-preview:toggle)
                  nmap <silent> <buffer> <C-p> <Plug>(fern-action-preview:auto:toggle)
                  nmap <silent> <buffer> <C-d> <Plug>(fern-action-preview:scroll:down:half)
                  nmap <silent> <buffer> <C-u> <Plug>(fern-action-preview:scroll:up:half)
                ]]
            end,
          })
        end,
      },
    },
  },
  {
    "nvim-tree/nvim-tree.lua",
    cmd = { "NvimTreeToggle", "NvimTreeFindFile" },
    config = function()
      local tree_cb = require("nvim-tree.config").nvim_tree_callback
      require("nvim-tree").setup {
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
              { key = "n", mode = "n", cb = tree_cb "create" },
              { key = "u", mode = "n", cb = tree_cb "dir_up" },
              { key = "-", cb = tree_cb "dir_up" },
              { key = "<CR>", cb = tree_cb "edit" },
              { key = "o", mode = "n", cb = tree_cb "cd" },
              { key = "<C-v>", cb = tree_cb "vsplit" },
              { key = "<C-x>", cb = tree_cb "split" },
              { key = "<C-t>", cb = tree_cb "tabnew" },
              { key = "<", cb = tree_cb "prev_sibling" },
              { key = ">", cb = tree_cb "next_sibling" },
              { key = "P", cb = tree_cb "parent_node" },
              { key = "<BS>", cb = tree_cb "close_node" },
              { key = "<S-CR>", cb = tree_cb "close_node" },
              { key = "<Tab>", cb = tree_cb "preview" },
              { key = "K", cb = tree_cb "first_sibling" },
              { key = "J", cb = tree_cb "last_sibling" },
              { key = "I", cb = tree_cb "toggle_ignored" },
              { key = "H", cb = tree_cb "toggle_dotfiles" },
              { key = "R", cb = tree_cb "refresh" },
              { key = "D", cb = tree_cb "remove" },
              { key = "d", cb = tree_cb "trash" },
              { key = "r", cb = tree_cb "rename" },
              { key = "<C->", cb = tree_cb "full_rename" },
              { key = "x", cb = tree_cb "cut" },
              { key = "c", cb = tree_cb "copy" },
              { key = "p", cb = tree_cb "paste" },
              { key = "y", cb = tree_cb "copy_name" },
              { key = "Y", cb = tree_cb "copy_path" },
              { key = "gy", cb = tree_cb "copy_absolute_path" },
              { key = "[c", cb = tree_cb "prev_git_item" },
              { key = "}c", cb = tree_cb "next_git_item" },
              { key = "<C-k>", cb = tree_cb "toggle_file_info" },
              { key = "q", cb = tree_cb "close" },
              { key = "?", cb = tree_cb "toggle_help" },
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
      }
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
    "monaqa/dial.nvim",
    -- lazy-load on keys
    -- mode is `n` by default. For more advanced options, check the section on key mappings
    keys = { "<C-a>", { "<C-x>", mode = "n" } },
  },
  {
    "uga-rosa/ccc.nvim",
    event = { "BufReadPre" },
    config = function()
      local ccc = require "ccc"
      ccc.setup {
        highlighter = {
          auto_enable = true,
          lsp = true,
        },
      }
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
      vim.cmd [[
          let g:asterisk#keeppos = 1
        ]]
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
      require("toggleterm").setup {
        size = 20,
        open_mapping = [[<c-t>]],
        shade_filetypes = {},
        shade_terminals = false,
        direction = "float",
        insert_mappings = true,
        close_on_exit = false,
        persist_size = false,
        persist_mode = false,
      }
    end,
  },
  "moll/vim-bbye",
  "godlygeek/tabular",
  {
    "airblade/vim-rooter",
    config = function()
      vim.g.rooter_patterns = { ".git", "Cargo.toml", "package.json" }
    end,
  },
  "machakann/vim-sandwich",
  "simeji/winresizer",
  {
    "windwp/nvim-autopairs",
    event = {"InsertEnter", "CmdlineEnter"},
    config = function()
      require("nvim-autopairs").setup {
        map_cr = true,
      }
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
  "nicwest/vim-camelsnek",
  "pechorin/any-jump.vim",
  {
    "phaazon/hop.nvim",
    keys = {
      {"f", "<cmd>lua require'hop'.hint_char1({ direction = nil, current_line_only = true })<cr>", "n", desc = "hop hint char"},
      {"f", "<cmd>lua require'hop'.hint_char1({ direction = nil, current_line_only = true, inclusive_jump = true })<cr>", "o", desc = "hop hint char(inclusive_jump)"},
      {"L", "<cmd>lua require'hop'.hint_lines_skip_whitespace({ current_line_only = false })<cr>", {"n", "v"}, desc = "hop lines"},
      {"mc", "<cmd>lua require'hop'.hint_char1({ direction = nil, current_line_only = false })<cr>", "n", desc = "hop hint char(current_line_only=false)"},
      {"mw", "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.BEGIN })<cr>", {"n", "v"}, desc = "hop hint words(BEGIN)"},
      {"mw", "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.BEGIN, inclusive_jump = true })<cr>", "o", desc = "hop hint words(BEGIN)"},
      {"mW", "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END })<cr>", {"n", "v"}, desc = "hop hint words(END)"},
      {"mW", "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END, inclusive_jump = true })<cr>", "o", desc = "hop hint words(END)"},
    },
    config = function()
      require("hop").setup {
        keys = "etovxqpdygfblzhckisuran",
        jump_on_sole_occurrence = false,
      }
    end,
  },
  "mtdl9/vim-log-highlighting",
  {
    "fannheyward/telescope-coc.nvim",
    enabled = function()
      return vim.g.lsp_client_type == "coc"
    end,
  },
  {
    "nvim-telescope/telescope.nvim",
    lazy = true,
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
      },
      "nvim-telescope/telescope-ui-select.nvim",
    },
    config = function()
      local telescope = require "telescope"
      telescope.setup {
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
        extensions = {
          ["ui-select"] = {
            require("telescope.themes").get_dropdown {
              -- even more opts
            },

            -- pseudo code / specification for writing custom displays, like the one
            -- for "codeactions"
            specific_opts = {
              --   [kind] = {
              --     make_indexed = function(items) -> indexed_items, width,
              --     make_displayer = function(widths) -> displayer
              --     make_display = function(displayer) -> function(e)
              --     make_ordinal = function(e) -> string
              --   },
              --   -- for example to disable the custom builtin "codeactions" display
              --      do the following
              codeactions = false,
            },
          },
        },
        fzf = {
          fuzzy = true, -- false will only do exact matching
          override_generic_sorter = true, -- override the generic sorter
          override_file_sorter = true, -- override the file sorter
          case_mode = "smart_case", -- or "ignore_case" or "respect_case"
          -- the default case_mode is "smart_case"
        },
      }
      telescope.load_extension "ui-select"
      telescope.load_extension "fzf"
      if vim.g.lsp_client_type == "coc" then
        telescope.load_extension "coc"
      end
    end,
  },

  -- Git
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("gitsigns").setup {
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
      }
    end,
  },
  {
    "sindrets/diffview.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local actions = require "diffview.actions"
      require("diffview").setup {
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
      }
      vim.api.nvim_set_keymap("n", "<leader>gdd", "<cmd>DiffviewOpen<cr>", require("scripts/util").keymaps.default_opt)
      vim.api.nvim_set_keymap(
        "n",
        "<leader>gdr",
        "<cmd>DiffviewFileHistory .<cr>",
        require("scripts/util").keymaps.default_opt
      )
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
      local neogit = require "neogit"
      neogit.setup {
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
      }
    end,
  },
  {
    "lambdalisue/gina.vim",
    enabled = function()
      return vim.g.git_client_type == "gina"
    end,
    config = function()
      vim.keymap.set("n", "<leader>gs", "<cmd>Gina status --opener=vsplit<cr>", {})
      vim.keymap.set("n", "<leader>gl", "<cmd>Gina log --opener=vsplit<cr>", {})
      vim.api.nvim_call_function(
        "gina#custom#mapping#nmap",
        { "status", "c", "<cmd>Gina commit --restore<cr>", { noremap = 1, silent = 1 } }
      )
      vim.api.nvim_call_function(
        "gina#custom#mapping#nmap",
        { "status", "P", "<cmd>Gina push<cr>", { noremap = 1, silent = 1 } }
      )
    end,
  },

  -- Snippet
  "mattn/vim-sonictemplate",

  -- LSP
  {
    "williamboman/mason.nvim",
    cmd = { "Mason", "MasonLog", "MasonInstall", "MasonUninstall", "MasonUninstallAll" },
    config = function()
      require("mason").setup {
        ui = {
          border = "rounded",
          icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗",
          },
        },
      }
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    event = { "BufReadPre" },
    config = function()
      require("mason-lspconfig").setup {
        ensure_installed = {
          "sumneko_lua",
          "rust_analyzer",
          "cssls",
          "dockerls",
          "eslint",
          "gopls",
          "html",
          "jsonls",
          "tsserver",
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
      }
    end,
  },
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    dependencies = {
      {
        "simrat39/rust-tools.nvim",
      },
      {
        "someone-stole-my-name/yaml-companion.nvim",
        dependencies = {
          "nvim-lua/plenary.nvim",
          "nvim-telescope/telescope.nvim",
        },
        config = function()
          require("telescope").load_extension "yaml_schema"
        end,
      },
      {
        "folke/neodev.nvim",
      },
      {
        "utilyre/barbecue.nvim",
        dependencies = {
          "kyazdani42/nvim-web-devicons", -- optional
          "smiteshp/nvim-navic",
        },
        config = function()
          local barbecue = require "barbecue"
          barbecue.setup {
            ---whether to create winbar updater autocmd
            ---@type boolean
            create_autocmd = true,

            ---buftypes to enable winbar in
            ---@type table
            include_buftypes = { "" },

            ---returns a string to be shown at the end of winbar
            ---@param bufnr number
            ---@return string
            custom_section = function(bufnr)
              return ""
            end,

            ---:help filename-modifiers
            modifiers = {
              ---@type string
              dirname = ":~:.",

              ---@type string
              basename = "",
            },

            ---icons used by barbecue
            ---@type table<string, string>
            symbols = {
              ---entry separator
              ---@type string
              separator = "",
              ---modification indicator
              ---`false` to disable
              ---@type false|string
              modified = false,
            },

            kinds = {
              ---@type string
              File = "",

              ---@type string
              Package = "",

              ---@type string
              Module = "",

              ---@type string
              Namespace = "",

              ---@type string
              Class = "",

              ---@type string
              Constructor = "",

              ---@type string
              Field = "",

              ---@type string
              Property = "",

              ---@type string
              Method = "",

              ---@type string
              Struct = "",

              ---@type string
              Event = "",

              ---@type string
              Interface = "",

              ---@type string
              Enum = "",

              ---@type string
              EnumMember = "",

              ---@type string
              Constant = "",

              ---@type string
              Function = "",

              ---@type string
              TypeParameter = "",

              ---@type string
              Variable = "",

              ---@type string
              Operator = "",

              ---@type string
              Null = "",

              ---@type string
              Boolean = "",

              ---@type string
              Number = "",

              ---@type string
              String = "",

              ---@type string
              Key = "",

              ---@type string
              Array = "",

              ---@type string
              Object = "",
            },
          }
        end,
      },
    },
    config = function()
      require "lsp_settings"
    end,
  },
  {
    "b0o/schemastore.nvim",
    event = { "BufReadPre" },
    dependencies = { "neovim/nvim-lspconfig" },
  },
  {
    "jose-elias-alvarez/null-ls.nvim",
    event = { "BufReadPre" },
    dependencies = { "neovim/nvim-lspconfig", "nvim-lua/plenary.nvim" },
    config = function()
      local data_dir = vim.fn.stdpath "data" .. "/cspell"
      local cspell_dic = {
        dotfiles = {
          name = "dotfiles",
          path = vim.fn.stdpath "config" .. "/cspell/dotfiles.txt",
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

      local null_ls = require "null-ls"

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
                local w = msg:match "%b()"
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

      null_ls.setup {
        sources = {
          null_ls.builtins.diagnostics.cspell.with {
            extra_args = { "--config", vim.fn.stdpath "config" .. "/cspell/cspell.json" },
            disabled_filetypes = { "NvimTree" },
            diagnostics_postprocess = function(diagnostic)
              -- レベルをWARNに変更（デフォルトはERROR）
              diagnostic.severity = vim.diagnostic.severity["WARN"]
            end,
            condition = function()
              -- cspellが実行できるときのみ有効
              return vim.fn.executable "cspell" > 0
            end,
            timeout = 50000,
          },
          null_ls.builtins.code_actions.cspell,
          null_ls.builtins.code_actions.shellcheck,
          null_ls.builtins.diagnostics.flake8.with {
            extra_args = { "--max-line-length", "120" },
          },
          null_ls.builtins.diagnostics.mypy,
          null_ls.builtins.diagnostics.golangci_lint.with {
            timeout = 50000,
          },
          null_ls.builtins.formatting.prettier.with {
            timeout = 50000,
          },
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
      }
    end,
  },
  {
    "hrsh7th/nvim-cmp",
    event = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      { "windwp/nvim-autopairs" },
      { "onsails/lspkind.nvim" },
      { "hrsh7th/vim-vsnip" },
      {
        "hrsh7th/cmp-vsnip",
        dependencies = "rafamadriz/friendly-snippets",
        config = function()
          vim.cmd [[
              " Expand
              imap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'
              smap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'

              " Expand or jump
              imap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
              smap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'

              " Jump forward or backward
              imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
              smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
              imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
              smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

              " Select or cut text to use as $TM_SELECTED_TEXT in the next snippet.
              " See https://github.com/hrsh7th/vim-vsnip/pull/50
              nmap        s   <Plug>(vsnip-select-text)
              xmap        s   <Plug>(vsnip-select-text)
              nmap        S   <Plug>(vsnip-cut-text)
              xmap        S   <Plug>(vsnip-cut-text)

              " If you want to use snippet for multiple filetypes, you can `g:vsnip_filetypes` for it.
              let g:vsnip_filetypes = {}
              let g:vsnip_filetypes.javascriptreact = ['javascript']
              let g:vsnip_filetypes.typescriptreact = ['typescript']
            ]]
        end,
      },
      { "saadparwaiz1/cmp_luasnip" },
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-path" },
      { "hrsh7th/cmp-cmdline" },
      {
        "petertriho/cmp-git",
        dependencies = "nvim-lua/plenary.nvim",
        config = function()
          require("cmp_git").setup {
            filetypes = { "NeogitCommitMessage", "gitcommit" },
          }
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
      local cmp = require "cmp"
      local types = require "cmp.types"
      local compare = require "cmp.config.compare"
      local lspkind = require "lspkind"
      cmp.setup {
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
            compare.offset,
            compare.exact,
            -- compare.scopes,
            compare.score,
            compare.recently_used,
            compare.locality,
            compare.kind,
            compare.sort_text,
            compare.length,
            compare.order,
          },
        },
        -- You should change this example to your chosen snippet engine.
        snippet = {
          expand = function(args)
            vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
            -- luasnip.lsp_expand(args.body) -- For `luasnip` users.
            -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
            -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
          end,
        },

        -- You must set mapping.
        mapping = cmp.mapping.preset.insert {
          ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
          ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
          ["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "s", "c" }),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping {
            i = cmp.mapping.abort(),
            c = cmp.mapping.close(),
          },
          ["<CR>"] = cmp.mapping.confirm { select = false },
        },

        -- You should specify your *installed* sources.
        sources = cmp.config.sources {
          {
            name = "nvim_lsp",
            priority = 10,
            max_item_count = 50,
          },
          { name = "nvim_lsp_signature_help" },
          {
            name = "vsnip",
            -- name = "luasnip",
            priority = 11,
            max_item_count = 50,
          },
          {
            name = "path",
            max_item_count = 20,
          },
          {
            name = "nvim_lua",
            priority = 11,
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
        },
        formatting = {
          format = lspkind.cmp_format {
            mode = "symbol_text",
            menu = {
              buffer = "[BUF]",
              nvim_lsp = "[LSP]",
              path = "[PATH]",
              vsnip = "[SNIP]",
              luasnip = "[SNIP]",
              nvim_lua = "[LUA]",
            },
          },
        },
        experimental = {
          native_menu = false,
          ghost_text = true,
        },
      }

      -- local cmp_autopairs = require "nvim-autopairs.completion.cmp"
      -- cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

      require "scripts/cmp/conventionalprefix"
      cmp.setup.filetype({ "NeogitCommitMessage", "gitcommit" }, {
        sources = cmp.config.sources {
          { name = "conventionalprefix" },
          { name = "git" },
        },
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
    "folke/trouble.nvim",
    dependencies = { "neovim/nvim-lspconfig" },
    cmd = { "Trouble", "TroubleToggle", "TroubleClose", "TroubleRefresh" },
    config = function()
      require("trouble").setup {
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
      }
    end,
  },
  {
    "akinsho/flutter-tools.nvim",
    dependencies = { "neovim/nvim-lspconfig" },
    ft = { "dart" },
  },
  {
    "neoclide/coc.nvim",
    enabled = function()
      return vim.g.lsp_client_type == "coc"
    end,
    build = "yarn install --frozen-lockfile",
    dependencies = { "rafcamlet/coc-nvim-lua" },
  },
  {
    "folke/which-key.nvim",
    lazy = false,
    config = function()
      local wk = require "which-key"
      wk.setup {
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
      }

      -- mode: n
      wk.register {
        ["R"] = { "<Plug>(operator-replace)", "Replace" },
        ["<leader>"] = {
          q = { "<cmd>Bdelete<CR>", "Delete Buffer" },
          Q = { "<cmd>Bdelete!<CR>", "Delete Buffer!" },
        },
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
        ["<leader>f"] = {
          name = "+Explorer",
          t = { "<cmd>NvimTreeToggle<cr>", "Toggle" },
          r = { "<cmd>NvimTreeRefresh<cr>", "Refresh" },
          f = { "<cmd>NvimTreeFindFile<cr>", "Focus File" },
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
            g = { '<cmd>lua require("telescope.builtin").live_grep{ glob_pattern = "!.git" }<CR>', "Live Grep" },
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
          f = { 'require("neogen").generate { type = "func" }', "Generate doc comment for function" },
          F = { 'require("neogen").generate { type = "file" }', "Generate doc comment for file" },
          t = { 'require("neogen").generate { type = "type" }', "Generate doc comment for type" },
          c = { 'require("neogen").generate { type = "class" }', "Generate doc comment for class" },
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
      }

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

      if vim.g.lsp_client_type == "neovim" then
        wk.register({
          ["g"] = {
            name = "+LSP",
            r = { "<cmd>Telescope lsp_references<CR>", "References" },
            i = { "<cmd>Telescope lsp_implementations<CR>", "Implementations" },
            d = { "<cmd>Telescope lsp_definitions<CR>", "Definition" },
            D = { "<cmd>Telescope lsp_type_definitions<CR>", "Type Definition" },
          },
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
            F = { "<cmd>lua vim.lsp.buf.format{async = true}<CR>", "Format" },
          },
          ["<leader>ac"] = { "<cmd>lua vim.lsp.buf.code_action()<CR>", "Code Action" },
          ["<leader>d"] = {
            name = "+Diagnostics",
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
          ["<leader>rn"] = { "<cmd>lua vim.lsp.buf.rename()<CR>", "Rename" },
        }, { mode = "n" })
        wk.register({
          ["<leader>ac"] = {
            "<cmd>lua vim.lsp.buf.code_action()<CR>",
            "Code Action",
          },
        }, { mode = "v" })
      elseif vim.g.lsp_client_type == "coc" then
        wk.register {
          ["<lesder>sd"] = { "<cmd>Telescope coc diagnostics<CR>", "Diagnostics" },
          ["<lesder>sD"] = { "<cmd>Telescope coc workspace_diagnostics<CR>", "Workspace Diagnostics" },
          ["<lesder>ca"] = { "<cmd>Telescope coc code_actions<CR>", "Code Actions" },
          ["g"] = {
            name = "+LSP",
            r = { "<cmd>Telescope coc references<CR>", "References" },
            i = { "<cmd>Telescope coc implementations<CR>", "Implementations" },
            y = { "<cmd>Telescope coc type_definitions<CR>", "Type Definitions" },
          },
        }
      end
    end,
  },

  -- ColorScheme
  {
    "lmburns/kimbox",
    lazy = true,
    config = function()
      require("kimbox").setup {
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
      }
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    lazy = true,
    config = function()
      require("kanagawa").setup {
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
      }
    end,
  },
  {
    "rmehri01/onenord.nvim",
    lazy = true,
    config = function()
      require("onenord").setup {
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
      }
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
      require("tokyonight").setup {
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
      }
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
      local nightfox = require "nightfox"
      nightfox.setup {
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
      }
      -- nightfox.load()
    end,
  },

  -- Local plugins
  {
    -- "tkmpypy/chowcho.nvim",
    dir = "~/ghq/github.com/tkmpypy/chowcho.nvim",
    config = function()
      require("chowcho").setup {
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
      }
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
  performance = {
    cache = {
      enabled = true,
      -- disable_events = {},
    },
    checker = { enabled = true },
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
