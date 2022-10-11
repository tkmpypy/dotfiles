pcall(require, "impatient")

local fn = vim.fn
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
local packer_compiled_path = fn.stdpath "config" .. "/lua/packer_compiled.lua"

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system {
    "git",
    "clone",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
end

local packer = require "packer"
local packer_util = require "packer.util"

vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
    autocmd User PackerCompileDone silent! lua require("packer_compiled")
  augroup end
]]

if fn.empty(fn.glob(packer_compiled_path)) == 0 then
  require "packer_compiled"
end

-- See https://github.com/wbthomason/packer.nvim/issues/202#issuecomment-826481883
packer.init {
  max_jobs = 50,
}
packer.startup {
  function(use)
    -- Packer can manage itself as an optional plugin
    use { "wbthomason/packer.nvim" }

    -- perf
    use {
      "lewis6991/impatient.nvim",
      config = function()
        require("impatient").enable_profile()
      end,
    }
    use {
      "antoinemadec/FixCursorHold.nvim",
      config = function()
        vim.g.cursorhold_updatetime = 300
      end,
    }

    -- ColorScheme
    use {
      "rebelot/kanagawa.nvim",
      config = function()
        require("kanagawa").setup {
          undercurl = true, -- enable undercurls
          commentStyle = { italic = true },
          functionStyle = {},
          keywordStyle = { italic = true },
          statementStyle = { bold = true },
          typeStyle = {},
          variablebuiltinStyle = { italic = true },
          specialReturn = true, -- special highlight for the return keyword
          specialException = true, -- special highlight for exception handling keywords
          transparent = true, -- do not set background color
          dimInactive = false, -- dim inactive window `:h hl-NormalNC`
          globalStatus = true, -- adjust window separators highlight for laststatus=3
          colors = {},
          overrides = {},
        }
      end,
    }
    use {
      "rmehri01/onenord.nvim",
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
          custom_highlights = {}, -- Overwrite default highlight groups
          custom_colors = {}, -- Overwrite default colors
        }
      end,
    }
    use {
      "sainnhe/edge",
      config = function()
        vim.g.edge_style = "aura"
        vim.g.edge_enable_italic = true
        vim.g.edge_disable_italic_comment = false
        vim.g.edge_current_word = "bold"
        vim.g.edge_transparent_background = true
      end,
    }
    use {
      "sainnhe/everforest",
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
    }
    use {
      "folke/tokyonight.nvim",
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
    }
    use {
      "eddyekofo94/gruvbox-flat.nvim",
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
    }
    use {
      "EdenEast/nightfox.nvim",
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
    }

    if vim.g.use_treesitter then
      use {
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        requires = {
          "yioneko/nvim-yati",
          "nvim-treesitter/nvim-treesitter-textobjects",
          "windwp/nvim-ts-autotag",
          "p00f/nvim-ts-rainbow",
          "JoosepAlviste/nvim-ts-context-commentstring",
        },
        config = function()
          require("nvim-treesitter.configs").setup {
            highlight = {
              enable = true,
              disable = { "lua" },
              additional_vim_regex_highlighting = false,
            },
            yati = { enable = true },
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
            textobjects = {
              move = {
                enable = true,
                set_jumps = true, -- whether to set jumps in the jumplist
              },
              select = {
                enable = true,
                -- Automatically jump forward to textobj, similar to targets.vim
                lookahead = true,
                include_surrounding_whitespace = true,
              },
            },
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
      }
      use {
        "b3nj5m1n/kommentary",
        opt = true,
        event = { "BufEnter" },
        config = function()
          local config = require "kommentary.config"
          config.use_extended_mappings()
          config.configure_language("default", {
            ignore_whitespace = true,
            use_consistent_indentation = true,
            prefer_single_line_comments = true,
          })
        end,
      }
      use {
        "danymat/neogen",
        opt = true,
        event = { "BufEnter" },
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
          vim.keymap.set("n", "<leader>ncf", function()
            require("neogen").generate { type = "func" }
          end, {})
          vim.keymap.set("n", "<leader>nct", function()
            require("neogen").generate { type = "type" }
          end, {})
          vim.keymap.set("n", "<leader>ncc", function()
            require("neogen").generate { type = "class" }
          end, {})
        end,
        requires = "nvim-treesitter/nvim-treesitter",
      }
    end

    -- Languages
    use { "hashivim/vim-terraform", opt = true, ft = { "terraform" } }
    use { "uarun/vim-protobuf", opt = true, ft = { "proto" } }
    use { "euclidianAce/BetterLua.vim", opt = true, ft = { "lua" } }
    use {
      "towolf/vim-helm",
      opt = true,
      ft = { "helm", "yaml" },
      config = function()
        vim.cmd [[autocmd BufRead,BufNewFile */templates/*.yml,*/templates/*.yaml,*/templates/*.tpl set ft=helm]]
      end,
    }
    use { "aklt/plantuml-syntax", opt = true, ft = { "plantuml" } }

    -- runner
    use { "vim-test/vim-test" }
    use { "thinca/vim-quickrun" }

    -- UI
    use {
      "levouh/tint.nvim",
      config = function()
        require("tint").setup {
          tint = -45, -- Darken colors, use a positive value to brighten
          saturation = 0.6, -- Saturation to preserve
          transforms = require("tint").transforms.SATURATE_TINT, -- Showing default behavior, but value here can be predefined set of transforms
          tint_background_colors = false, -- Tint background portions of highlight groups
          highlight_ignore_patterns = { "WinSeparator", "Status.*" }, -- Highlight group patterns to ignore, see `string.find`
          window_ignore_function = function(winid)
            local bufid = vim.api.nvim_win_get_buf(winid)
            local buftype = vim.api.nvim_buf_get_option(bufid, "buftype")
            local floating = vim.api.nvim_win_get_config(winid).relative ~= ""

            -- Do not tint `terminal` or floating windows, tint everything else
            return buftype == "terminal" or floating
          end,
        }
      end,
    }
    use {
      "petertriho/nvim-scrollbar",
      requires = { "kevinhwang91/nvim-hlslens" },
      opt = true,
      event = { "BufEnter" },
      config = function()
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
    }
    use {
      "lukas-reineke/indent-blankline.nvim",
      opt = true,
      event = { "BufEnter" },
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
          filetype_exclude = { "startify", "alpha", "NvimTree", "notify", "packer", "lsp-installer", "windline" },
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
    }
    use { "kyazdani42/nvim-web-devicons" }
    use {
      "nvim-lualine/lualine.nvim",
      opt = true,
      event = { "ColorScheme" },
      requires = { { "kyazdani42/nvim-web-devicons", opt = true } },
      disable = false,
      config = function()
        local util = require "scripts/util"
        local lualine_utils = require "lualine.utils.utils"
        local theme = require "lualine/themes/onedark"
        require("lualine").setup {
          options = {
            icons_enabled = true,
            theme = theme,
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
    }
    use {
      "windwp/windline.nvim",
      disable = true,
      config = function()
        local windline = require "windline"
        local helper = require "windline.helpers"
        local b_components = require "windline.components.basic"
        --- @diagnostic disable-next-line: undefined-field
        local W = _G.WindLine
        local state = W.state

        local lsp_comps = require "windline.components.lsp"
        local git_comps = require "windline.components.git"

        local hl_list = {
          Inactive = { "InactiveFg", "InactiveBg" },
          Active = { "ActiveFg", "ActiveBg" },
        }
        local basic = {}

        local breakpoint_width = 90
        basic.divider = { b_components.divider, hl_list.Active }
        basic.bg = { " ", hl_list.Active }

        local colors_mode = {
          Normal = { "red", "ActiveBg" },
          Insert = { "green", "ActiveBg" },
          Visual = { "yellow", "ActiveBg" },
          Replace = { "blue_light", "ActiveBg" },
          Command = { "magenta", "ActiveBg" },
        }

        basic.separate = { " ", hl_list.Active }
        basic.vi_mode = {
          name = "vi_mode",
          hl_colors = colors_mode,
          text = function()
            return { { "  ", state.mode[2] } }
          end,
        }
        basic.square_mode = {
          hl_colors = colors_mode,
          text = function()
            return { { "▊", state.mode[2] } }
          end,
        }
        basic.scroll_bar = {
          text = function()
            local util = require "scripts/util"
            return {
              { util.buffer.scroll_bar(), "" },
            }
          end,
        }

        basic.lsp_diagnos = {
          name = "diagnostic",
          hl_colors = {
            red = { "red", "ActiveBg" },
            yellow = { "yellow", "ActiveBg" },
            blue = { "blue", "ActiveBg" },
            cyan = { "cyan", "ActiveBg" },
          },
          width = breakpoint_width,
          text = function(bufnr)
            if lsp_comps.check_lsp(bufnr) then
              return {
                { lsp_comps.lsp_error { format = " %s", show_zero = true }, "red" },
                { lsp_comps.lsp_warning { format = "  %s", show_zero = true }, "yellow" },
                { lsp_comps.lsp_info { format = "  %s", show_zero = true }, "blue" },
                { lsp_comps.lsp_hint { format = "  %s", show_zero = true }, "cyan" },
              }
            end
            return ""
          end,
        }
        basic.file = {
          name = "file",
          hl_colors = {
            default = hl_list.Black,
            white = { "white", "ActiveBg" },
            magenta = { "magenta", "ActiveBg" },
          },
          text = function(_, _, _)
            return {
              { b_components.cache_file_icon(), "magenta" },
              { " ", "" },
              { b_components.cache_file_name("[No Name]", "unique"), "magenta" },
              { " ", "" },
              { b_components.file_modified " ", "magenta" },
            }
          end,
        }
        basic.file_right = {
          hl_colors = {
            default = hl_list.Active,
            white = { "white", "ActiveBg" },
            magenta = { "magenta", "ActiveBg" },
          },
          text = function(_, _, _)
            return {
              { b_components.line_col_lua, { "InactiveFg" } },
            }
          end,
        }
        basic.git = {
          name = "git",
          hl_colors = {
            green = { "green", "ActiveBg" },
            red = { "red", "ActiveBg" },
            blue = { "blue", "ActiveBg" },
          },
          width = breakpoint_width,
          text = function(bufnr)
            if git_comps.is_git(bufnr) then
              return {
                { " ", "" },
                { git_comps.diff_added { format = " %s", show_zero = true }, "green" },
                { git_comps.diff_removed { format = "  %s", show_zero = true }, "red" },
                { git_comps.diff_changed { format = " 柳%s", show_zero = true }, "blue" },
              }
            end
            return ""
          end,
        }

        local quickfix = {
          filetypes = { "qf", "Trouble" },
          active = {
            { "🚦 Quickfix ", { "white", "black" } },
            { helper.separators.slant_right, { "black", "black_light" } },
            {
              function()
                ---@diagnostic disable-next-line: undefined-field
                return vim.fn.getqflist({ title = 0 }).title
              end,
              { "cyan", "black_light" },
            },
            { " Total : %L ", { "cyan", "black_light" } },
            { helper.separators.slant_right, { "black_light", "InactiveBg" } },
            { " ", { "InactiveFg", "InactiveBg" } },
            basic.divider,
            { helper.separators.slant_right, { "InactiveBg", "black" } },
            { "🧛 ", { "white", "black" } },
          },

          always_active = true,
          show_last_status = true,
        }

        local explorer = {
          filetypes = { "fern", "NvimTree", "lir" },
          active = {
            { "  ", { "black", "red" } },
            { helper.separators.slant_right, { "red", "ActiveBg" } },
            { b_components.divider, "" },
            { b_components.file_name "", { "white", "ActiveBg" } },
          },
          always_active = true,
          show_last_status = true,
        }
        local default = {
          filetypes = { "default" },
          active = {
            basic.square_mode,
            basic.vi_mode,
            basic.separate,
            basic.file,
            basic.separate,
            { lsp_comps.lsp_name(), { "green", "ActiveBg" }, breakpoint_width },
            basic.separate,
            basic.lsp_diagnos,
            basic.divider,
            { git_comps.git_branch(), { "magenta", "ActiveBg" }, breakpoint_width },
            basic.git,
            basic.file_right,
            basic.scroll_bar,
            basic.separate,
            basic.square_mode,
          },
          inactive = {
            { b_components.full_file_name, hl_list.Inactive },
            basic.divider,
            basic.divider,
            { b_components.line_col_lua, hl_list.Inactive },
          },
        }
        local statuslines = {
          default,
          quickfix,
          explorer,
        }

        if vim.g.lsp_client_type == "neovim" then
          local navic = require "nvim-navic"
          local winbar = {
            filetypes = { "winbar" },
            active = {
              { " " },
              { "" },
              {
                function(_)
                  return navic.get_location()
                end,
              },
            },
            inactive = {},
            enable = function(bufnr, winid)
              return navic.is_available()
            end, --a function to disable winbar on some window or filetype
          }

          table.insert(statuslines, winbar)
        end

        windline.setup {
          global_skip_filetypes = {
            "NvimTree",
            "lir",
            "fern",
          },
          colors_name = function(colors)
            -- print(vim.inspect(colors))
            -- ADD MORE COLOR HERE ----
            return colors
          end,
          statuslines = statuslines,
        }
      end,
    }
    use {
      "akinsho/bufferline.nvim",
      disable = true,
      requires = { "kyazdani42/nvim-web-devicons" },
      config = function()
        require("bufferline").setup {
          options = {
            view = "multiwindow", -- "multiwindow" | "default"
            numbers = "ordinal", -- "none" | "ordinal" | "buffer_id"
            buffer_close_icon = "",
            modified_icon = "●",
            close_icon = "",
            left_trunc_marker = "",
            right_trunc_marker = "",
            max_name_length = 18,
            tab_size = 18,
            show_buffer_close_icons = true,
            -- can also be a table containing 2 custom separators
            -- [focused and unfocused]. eg: { '|', '|' }
            separator_style = "thin", -- "slant" | "thick" | "thin" | { 'any', 'any' }
            enforce_regular_tabs = false,
            always_show_bufferline = true,
            sort_by = "extension",
          },
        }
      end,
    }
    use {
      "goolord/alpha-nvim",
      requires = { "kyazdani42/nvim-web-devicons" },
      config = function()
        local alpha = require "alpha"
        local theme = require "alpha.themes.startify"

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
        theme.section.header.val = header.val
        theme.section.header.opts = header.opts
        alpha.setup(theme.config)
        vim.cmd [[
            autocmd FileType alpha setlocal nofoldenable
        ]]
      end,
    }
    use { "liuchengxu/vista.vim" }
    use {
      "folke/noice.nvim",
      event = "VimEnter",
      requires = {
        "MunifTanjim/nui.nvim",
        "rcarriga/nvim-notify",
      },
      config = function()
        require("noice").setup {
          cmdline = {
            view = "cmdline_popup", -- view for rendering the cmdline. Change to `cmdline` to get a classic cmdline at the bottom
            opts = { buf_options = { filetype = "vim" } }, -- enable syntax highlighting in the cmdline
            icons = {
              ["/"] = { icon = " ", hl_group = "DiagnosticWarn" },
              ["?"] = { icon = " ", hl_group = "DiagnosticWarn" },
              [":"] = { icon = " ", hl_group = "DiagnosticInfo", firstc = false },
            },
          },
          history = {
            -- options for the message history that you get with `:Noice`
            view = "split",
            opts = { enter = true },
            filter = { event = "msg_show", ["not"] = { kind = { "search_count", "echo" } } },
          },
          popupmenu = {
            enabled = true, -- disable if you use something like cmp-cmdline
            backend = "nui", -- 'nui' | 'cmp'
          },
        }
      end,
    }

    -- explorer
    use {
      "lambdalisue/fern.vim",
      disable = true,
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
    }
    use {
      "kyazdani42/nvim-tree.lua",
      disable = false,
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
        }
      end,
    }

    -- Lua Utils
    use { "rafcamlet/nvim-luapad" }

    -- Utils
    use {
      "uga-rosa/ccc.nvim",
      event = { "VimEnter" },
      config = function()
        local ccc = require "ccc"
        ccc.setup {
          highlighter = {
            auto_enable = true,
            lsp = true,
          },
        }
      end,
    }
    use {
      "jbyuki/venn.nvim",
      config = function()
        -- venn.nvim: enable or disable keymappings
        function _G.Toggle_venn()
          local venn_enabled = vim.inspect(vim.b.venn_enabled)
          if venn_enabled == "nil" then
            vim.b.venn_enabled = true
            vim.cmd [[setlocal ve=all]]
            -- draw a line on HJKL keystokes
            vim.api.nvim_buf_set_keymap(0, "n", "J", "<C-v>j:VBox<CR>", { noremap = true })
            vim.api.nvim_buf_set_keymap(0, "n", "K", "<C-v>k:VBox<CR>", { noremap = true })
            vim.api.nvim_buf_set_keymap(0, "n", "L", "<C-v>l:VBox<CR>", { noremap = true })
            vim.api.nvim_buf_set_keymap(0, "n", "H", "<C-v>h:VBox<CR>", { noremap = true })
            -- draw a box by pressing "f" with visual selection
            vim.api.nvim_buf_set_keymap(0, "v", "f", ":VBox<CR>", { noremap = true })
          else
            vim.cmd [[setlocal ve=]]
            vim.cmd [[mapclear <buffer>]]
            vim.cmd [[IndentBlanklineDisable]]
            vim.b.venn_enabled = nil
          end
        end

        -- toggle keymappings for venn using <leader>v
        vim.api.nvim_set_keymap("n", "<leader>v", ":lua Toggle_venn()<CR>", { noremap = true })
      end,
    }
    use {
      "thinca/vim-qfreplace",
    }
    use {
      "haya14busa/vim-asterisk",
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
    }
    use {
      "rcarriga/nvim-notify",
      config = function()
        vim.notify = require "notify"
        vim.notify.setup {
          -- Animation style (see below for details)
          stages = "fade_in_slide_out",

          -- Default timeout for notifications
          timeout = 5000,

          -- For stages that change opacity this is treated as the highlight behind the window
          background_colour = "#000000",

          -- Icons for the different levels
          icons = {
            ERROR = "",
            WARN = "",
            INFO = "",
            DEBUG = "",
            TRACE = "✎",
          },
        }
      end,
    }
    use {
      "akinsho/toggleterm.nvim",
      config = function()
        require("toggleterm").setup {
          size = 20,
          open_mapping = [[<c-t>]],
          shade_filetypes = {},
          shade_terminals = true,
          direction = "horizontal",
          insert_mappings = true,
          close_on_exit = false,
        }
      end,
    }
    use {
      "moll/vim-bbye",
    }
    use { "godlygeek/tabular" }
    use { "airblade/vim-rooter" }
    use { "machakann/vim-sandwich" }
    use { "simeji/winresizer" }
    use {
      "windwp/nvim-autopairs",
      config = function()
        require("nvim-autopairs").setup {
          disable_filetype = { "TelescopePrompt" },
          ignored_next_char = string.gsub([[ [%w%%%'%[%"%.] ]], "%s+", ""),
          enable_moveright = true,
          enable_afterquote = true, -- add bracket pairs after quote
          enable_check_bracket_line = true, --- check bracket in same line
          check_ts = false,
        }
      end,
    }
    use {
      "iamcco/markdown-preview.nvim",
      run = function()
        vim.fn["mkdp#util#install"]()
      end,
      ft = { "markdown" },
    }
    use { "npxbr/glow.nvim", ft = { "markdown" } }
    use { "osyo-manga/vim-over" }
    use { "nicwest/vim-camelsnek" }
    use { "pechorin/any-jump.vim" }
    use {
      "phaazon/hop.nvim",
      config = function()
        require("hop").setup {
          keys = "etovxqpdygfblzhckisuran",
          jump_on_sole_occurrence = false,
        }
        vim.keymap.set(
          "n",
          "f",
          "<cmd>lua require'hop'.hint_char1({ direction = nil, current_line_only = true })<cr>",
          {}
        )
        vim.keymap.set(
          "o",
          "f",
          "<cmd>lua require'hop'.hint_char1({ direction = nil, current_line_only = true, inclusive_jump = true })<cr>",
          {}
        )
        vim.keymap.set(
          { "n", "v" },
          "L",
          "<cmd>lua require'hop'.hint_lines_skip_whitespace({ current_line_only = false })<cr>",
          {}
        )
        vim.keymap.set(
          "n",
          "mc",
          "<cmd>lua require'hop'.hint_char1({ direction = nil, current_line_only = false })<cr>",
          {}
        )
        vim.keymap.set(
          { "n", "v" },
          "mw",
          "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.BEGIN })<cr>",
          {}
        )
        vim.keymap.set(
          "o",
          "mw",
          "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.BEGIN, inclusive_jump = true })<cr>",
          {}
        )
        vim.keymap.set(
          { "n", "v" },
          "mW",
          "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END })<cr>",
          {}
        )
        vim.keymap.set(
          "o",
          "mW",
          "<cmd> lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END, inclusive_jump = true })<cr>",
          {}
        )
      end,
    }
    use { "mtdl9/vim-log-highlighting", opt = true }

    -- finder
    if vim.g.lsp_client_type == "coc" then
      use { "fannheyward/telescope-coc.nvim" }
    end

    use {
      "nvim-telescope/telescope.nvim",
      requires = {
        { "nvim-lua/plenary.nvim" },
        { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
        { "nvim-telescope/telescope-ui-select.nvim" },
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
    }
    use {
      "hoschi/yode-nvim",
      requires = { "nvim-lua/plenary.nvim" },
      config = function()
        require("yode-nvim").setup {}
      end,
    }

    -- Git
    use {
      "tanvirtin/vgit.nvim",
      disable = true,
      requires = {
        "nvim-lua/plenary.nvim",
      },
      config = function()
        require("vgit").setup {
          keymaps = {
            ["n <C-k>"] = "hunk_up",
            ["n <C-j>"] = "hunk_down",
            ["n <leader>ghs"] = "buffer_hunk_stage",
            ["n <leader>ghx"] = "buffer_hunk_reset",
            ["n <leader>ghp"] = "buffer_hunk_preview",
            ["n <leader>gbp"] = "buffer_blame_preview",
            ["n <leader>gbd"] = "buffer_diff_preview",
            ["n <leader>gbu"] = "buffer_reset",
            ["n <leader>gbs"] = "buffer_stage",
            ["n <leader>gbh"] = "buffer_history_preview",
            ["n <leader>gbg"] = "buffer_gutter_blame_preview",
            ["n <leader>gpl"] = "project_hunks_preview",
            ["n <leader>gpq"] = "project_hunks_qf",
            ["n <leader>gx"] = "toggle_diff_preference",
          },
          settings = {
            hls = {
              GitBackgroundPrimary = "NormalFloat",
              GitBackgroundSecondary = {
                gui = nil,
                fg = nil,
                bg = nil,
                sp = nil,
                override = false,
              },
              GitBorder = "LineNr",
              GitLineNr = "LineNr",
              GitComment = "Comment",
              GitSignsAdd = {
                gui = nil,
                fg = "#d7ffaf",
                bg = nil,
                sp = nil,
                override = false,
              },
              GitSignsChange = {
                gui = nil,
                fg = "#7AA6DA",
                bg = nil,
                sp = nil,
                override = false,
              },
              GitSignsDelete = {
                gui = nil,
                fg = "#e95678",
                bg = nil,
                sp = nil,
                override = false,
              },
              GitSignsAddLn = "DiffAdd",
              GitSignsDeleteLn = "DiffDelete",
              GitWordAdd = {
                gui = nil,
                fg = nil,
                bg = "#5d7a22",
                sp = nil,
                override = false,
              },
              GitWordDelete = {
                gui = nil,
                fg = nil,
                bg = "#960f3d",
                sp = nil,
                override = false,
              },
            },
            live_blame = {
              enabled = false,
              format = function(blame, git_config)
                local config_author = git_config["user.name"]
                local author = blame.author
                if config_author == author then
                  author = "You"
                end
                local time = os.difftime(os.time(), blame.author_time) / (60 * 60 * 24 * 30 * 12)
                local time_divisions = {
                  { 1, "years" },
                  { 12, "months" },
                  { 30, "days" },
                  { 24, "hours" },
                  { 60, "minutes" },
                  { 60, "seconds" },
                }
                local counter = 1
                local time_division = time_divisions[counter]
                local time_boundary = time_division[1]
                local time_postfix = time_division[2]
                while time < 1 and counter ~= #time_divisions do
                  time_division = time_divisions[counter]
                  time_boundary = time_division[1]
                  time_postfix = time_division[2]
                  time = time * time_boundary
                  counter = counter + 1
                end
                local commit_message = blame.commit_message
                if not blame.committed then
                  author = "You"
                  commit_message = "Uncommitted changes"
                  return string.format(" %s • %s", author, commit_message)
                end
                local max_commit_message_length = 255
                if #commit_message > max_commit_message_length then
                  commit_message = commit_message:sub(1, max_commit_message_length) .. "..."
                end
                return string.format(
                  " %s, %s • %s",
                  author,
                  string.format(
                    "%s %s ago",
                    time >= 0 and math.floor(time + 0.5) or math.ceil(time - 0.5),
                    time_postfix
                  ),
                  commit_message
                )
              end,
            },
            live_gutter = {
              enabled = false,
            },
            authorship_code_lens = {
              enabled = true,
            },
            screen = {
              diff_preference = "unified",
            },
            project_diff_preview = {
              keymaps = {
                buffer_stage = "s",
                buffer_unstage = "u",
                stage_all = "a",
                unstage_all = "d",
                reset_all = "r",
              },
            },
            signs = {
              priority = 10,
              definitions = {
                GitSignsAddLn = {
                  linehl = "GitSignsAddLn",
                  texthl = nil,
                  numhl = nil,
                  icon = nil,
                  text = "",
                },
                GitSignsDeleteLn = {
                  linehl = "GitSignsDeleteLn",
                  texthl = nil,
                  numhl = nil,
                  icon = nil,
                  text = "",
                },
                GitSignsAdd = {
                  texthl = "GitSignsAdd",
                  numhl = nil,
                  icon = nil,
                  linehl = nil,
                  text = "┃",
                },
                GitSignsDelete = {
                  texthl = "GitSignsDelete",
                  numhl = nil,
                  icon = nil,
                  linehl = nil,
                  text = "┃",
                },
                GitSignsChange = {
                  texthl = "GitSignsChange",
                  numhl = nil,
                  icon = nil,
                  linehl = nil,
                  text = "┃",
                },
              },
              usage = {
                screen = {
                  add = "GitSignsAddLn",
                  remove = "GitSignsDeleteLn",
                },
                main = {
                  add = "GitSignsAdd",
                  remove = "GitSignsDelete",
                  change = "GitSignsChange",
                },
              },
            },
            symbols = {
              void = "⣿",
            },
          },
        }
      end,
    }
    use {
      "lewis6991/gitsigns.nvim",
      requires = { "nvim-lua/plenary.nvim" },
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
          },
        }
      end,
    }
    use {
      "sindrets/diffview.nvim",
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
              single_file = {
                max_count = 512,
                follow = true,
              },
              multi_file = {
                max_count = 128,
                -- follow = false   -- `follow` only applies to single-file history
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
        vim.api.nvim_set_keymap(
          "n",
          "<leader>gdd",
          "<cmd>DiffviewOpen<cr>",
          require("scripts/util").keymaps.default_opt
        )
        vim.api.nvim_set_keymap(
          "n",
          "<leader>gdr",
          "<cmd>DiffviewFileHistory .<cr>",
          require("scripts/util").keymaps.default_opt
        )
      end,
    }

    if vim.g.git_client_type == "neogit" then
      use {
        "TimUntersberger/neogit",
        requires = { { "nvim-lua/plenary.nvim" }, { "sindrets/diffview.nvim" } },
        config = function()
          local neogit = require "neogit"
          neogit.setup {
            disable_signs = true,
            disable_context_highlighting = false,
            disable_commit_confirmation = true,
            auto_refresh = true,
            disable_builtin_notifications = false,
            -- Change the default way of opening neogit
            -- `tab`, `replace`, `floating`, `split`, `split_above`, `vsplit`
            kind = "split",
            -- Change the default way of opening the commit popup
            commit_popup = {
              kind = "split",
            },
            -- Change the default way of opening popups
            popup = {
              kind = "split",
            },
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

          vim.api.nvim_set_keymap(
            "n",
            "<leader>gs",
            "<cmd>Neogit kind=vsplit<cr>",
            require("scripts/util").keymaps.default_opt
          )
        end,
      }
    elseif vim.g.git_client_type == "gina" then
      use {
        "lambdalisue/gina.vim",
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
      }
    end
    use {
      "pwntester/octo.nvim",
      disable = true,
      config = function()
        require("octo").setup {
          date_format = "%Y %b %d %I:%M %p %Z", -- date format
          default_remote = { "upstream", "origin" }, -- order to try remotes
          reaction_viewer_hint_icon = "", -- marker for user reactions
          user_icon = " ", -- user icon
          timeline_marker = "", -- timeline marker
          timeline_indent = "2", -- timeline indentation
          right_bubble_delimiter = "", -- Bubble delimiter
          left_bubble_delimiter = "", -- Bubble delimiter
          github_hostname = "", -- GitHub Enterprise host
          snippet_context_lines = 4, -- number or lines around commented lines
          file_panel = {
            size = 10, -- changed files panel rows
            use_icons = true, -- use web-devicons in file panel
          },
          mappings = {
            issue = {
              close_issue = "<space>ic", -- close issue
              reopen_issue = "<space>io", -- reopen issue
              list_issues = "<space>il", -- list open issues on same repo
              reload = "<C-r>", -- reload issue
              open_in_browser = "<C-o>", -- open issue in browser
              add_assignee = "<space>aa", -- add assignee
              remove_assignee = "<space>ad", -- remove assignee
              add_label = "<space>la", -- add label
              remove_label = "<space>ld", -- remove label
              goto_issue = "<space>gi", -- navigate to a local repo issue
              add_comment = "<space>ca", -- add comment
              delete_comment = "<space>cd", -- delete comment
              next_comment = "]c", -- go to next comment
              prev_comment = "[c", -- go to previous comment
              react_hooray = "<space>rp", -- add/remove 🎉 reaction
              react_heart = "<space>rh", -- add/remove ❤️ reaction
              react_eyes = "<space>re", -- add/remove 👀 reaction
              react_thumbs_up = "<space>r+", -- add/remove 👍 reaction
              react_thumbs_down = "<space>r-", -- add/remove 👎 reaction
              react_rocket = "<space>rr", -- add/remove 🚀 reaction
              react_laugh = "<space>rl", -- add/remove 😄 reaction
              react_confused = "<space>rc", -- add/remove 😕 reaction
            },
            pull_request = {
              checkout_pr = "<space>po", -- checkout PR
              merge_pr = "<space>pm", -- merge PR
              list_commits = "<space>pc", -- list PR commits
              list_changed_files = "<space>pf", -- list PR changed files
              show_pr_diff = "<space>pd", -- show PR diff
              add_reviewer = "<space>va", -- add reviewer
              remove_reviewer = "<space>vd", -- remove reviewer request
              close_issue = "<space>ic", -- close PR
              reopen_issue = "<space>io", -- reopen PR
              list_issues = "<space>il", -- list open issues on same repo
              reload = "<C-r>", -- reload PR
              open_in_browser = "<C-o>", -- open PR in browser
              add_assignee = "<space>aa", -- add assignee
              remove_assignee = "<space>ad", -- remove assignee
              add_label = "<space>la", -- add label
              remove_label = "<space>ld", -- remove label
              goto_issue = "<space>gi", -- navigate to a local repo issue
              add_comment = "<space>ca", -- add comment
              delete_comment = "<space>cd", -- delete comment
              next_comment = "]c", -- go to next comment
              prev_comment = "[c", -- go to previous comment
              react_hooray = "<space>rp", -- add/remove 🎉 reaction
              react_heart = "<space>rh", -- add/remove ❤️ reaction
              react_eyes = "<space>re", -- add/remove 👀 reaction
              react_thumbs_up = "<space>r+", -- add/remove 👍 reaction
              react_thumbs_down = "<space>r-", -- add/remove 👎 reaction
              react_rocket = "<space>rr", -- add/remove 🚀 reaction
              react_laugh = "<space>rl", -- add/remove 😄 reaction
              react_confused = "<space>rc", -- add/remove 😕 reaction
            },
            review_thread = {
              goto_issue = "<space>gi", -- navigate to a local repo issue
              add_comment = "<space>ca", -- add comment
              add_suggestion = "<space>sa", -- add suggestion
              delete_comment = "<space>cd", -- delete comment
              next_comment = "]c", -- go to next comment
              prev_comment = "[c", -- go to previous comment
              select_next_entry = "]q", -- move to previous changed file
              select_prev_entry = "[q", -- move to next changed file
              close_review_tab = "<C-c>", -- close review tab
              react_hooray = "<space>rp", -- add/remove 🎉 reaction
              react_heart = "<space>rh", -- add/remove ❤️ reaction
              react_eyes = "<space>re", -- add/remove 👀 reaction
              react_thumbs_up = "<space>r+", -- add/remove 👍 reaction
              react_thumbs_down = "<space>r-", -- add/remove 👎 reaction
              react_rocket = "<space>rr", -- add/remove 🚀 reaction
              react_laugh = "<space>rl", -- add/remove 😄 reaction
              react_confused = "<space>rc", -- add/remove 😕 reaction
            },
            review_diff = {
              add_review_comment = "<space>ca", -- add a new review comment
              add_review_suggestion = "<space>sa", -- add a new review suggestion
              focus_files = "<leader>e", -- move focus to changed file panel
              toggle_files = "<leader>b", -- hide/show changed files panel
              next_thread = "]t", -- move to next thread
              prev_thread = "[t", -- move to previous thread
              select_next_entry = "]q", -- move to previous changed file
              select_prev_entry = "[q", -- move to next changed file
              close_review_tab = "<C-c>", -- close review tab
            },
            submit_win = {
              approve_review = "<C-a>", -- approve review
              comment_review = "<C-m>", -- comment review
              request_changes = "<C-r>", -- request changes review
              close_review_tab = "<C-c>", -- close review tab
            },
            file_panel = {
              next_entry = "j", -- move to next changed file
              prev_entry = "k", -- move to previous changed file
              select_entry = "<cr>", -- show selected changed file diffs
              refresh_files = "R", -- refresh changed files panel
              focus_files = "<leader>e", -- move focus to changed file panel
              toggle_files = "<leader>b", -- hide/show changed files panel
              select_next_entry = "]q", -- move to previous changed file
              select_prev_entry = "[q", -- move to next changed file
              close_review_tab = "<C-c>", -- close review tab
            },
          },
        }
        --[[ vim.api.nvim_set_keymap("n", "<leader>gil",
                          "<cmd>Octo issue list<cr>",
                          {silent = true, noremap = true})
      vim.api.nvim_set_keymap("n", "<leader>gpl",
                          "<cmd>Octo pr list<cr>",
                          {silent = true, noremap = true}) ]]
      end,
    }

    -- Snippet
    use { "mattn/vim-sonictemplate" }
    if vim.g.lsp_client_type == "neovim" then
      -- use neovim built-in
      use {
        "neovim/nvim-lspconfig",
        requires = {
          use {
            "williamboman/mason.nvim",
            requires = {
              "williamboman/mason-lspconfig.nvim",
            },
          },
          use {
            "someone-stole-my-name/yaml-companion.nvim",
            requires = {
              "nvim-lua/plenary.nvim",
              "nvim-telescope/telescope.nvim",
            },
            config = function()
              require("telescope").load_extension "yaml_schema"
            end,
          },
          use {
            "folke/lua-dev.nvim",
          },
          use {
            "utilyre/barbecue.nvim",
            disable = false,
            requires = {
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

                symbols = {
                  ---string to be shown at the start of winbar
                  ---@type string
                  prefix = " ",

                  ---entry separator
                  ---@type string
                  -- The sign between each entry
                  separator = "",

                  ---string to be shown when buffer is modified
                  ---@type string
                  modified = "",

                  ---string to be shown when context is available but empty
                  ---@type string
                  default_context = "…",
                },

                ---icons for different context entry kinds
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
          use {
            "simrat39/inlay-hints.nvim",
            disable = true,
            config = function()
              require("inlay-hints").setup {
                only_current_line = true,

                eol = {
                  right_align = false,
                  -- padding from the right if right_align is true
                  right_align_padding = 7,
                  parameter = {
                    separator = ", ",
                    format = function(hints)
                      return string.format("  (%s)", hints)
                    end,
                  },

                  type = {
                    separator = ", ",
                    format = function(hints)
                      return string.format("  (%s)", hints)
                    end,
                  },
                },
              }
            end,
          },
          use {
            "glepnir/lspsaga.nvim",
            branch = "main",
            config = function()
              local saga = require "lspsaga"
              saga.init_lsp_saga {
                -- Options with default value
                -- "single" | "double" | "rounded" | "bold" | "plus"
                border_style = "rounded",
                --the range of 0 for fully opaque window (disabled) to 100 for fully
                --transparent background. Values between 0-30 are typically most useful.
                saga_winblend = 20,
                diagnostic_header = { " ", " ", " ", "ﴞ " },
                code_action_icon = "💡",
                code_action_lightbulb = {
                  enable = false,
                  sign = false,
                  sign_priority = 20,
                  virtual_text = false,
                },
                -- finder icons
                finder_icons = {
                  def = "  ",
                  ref = "諭 ",
                  link = "  ",
                },
                -- preview lines of lsp_finder and definition preview
                max_preview_lines = 10,
                scroll_in_preview = {
                  scroll_down = "<C-f>",
                  scroll_up = "<C-b>",
                },
                finder_action_keys = {
                  open = "o",
                  vsplit = "s",
                  split = "i",
                  tabe = "t",
                  quit = "q",
                  scroll_down = "<C-f>",
                  scroll_up = "<C-b>", -- quit can be a table
                },
                code_action_keys = {
                  quit = "q",
                  exec = "<CR>",
                },
                rename_action_quit = "<C-c>",
                -- show symbols in winbar must nightly
                symbol_in_winbar = {
                  in_custom = false,
                  enable = false,
                  separator = "  ",
                  show_file = true,
                  click_support = false,
                },
                -- show outline
                show_outline = {
                  win_position = "right",
                  -- set the special filetype in there which in left like nvimtree neotree defx
                  left_with = "",
                  win_width = 30,
                  auto_enter = true,
                  auto_preview = true,
                  virt_text = "┃",
                  jump_key = "o",
                },
              }
            end,
          },
        },
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
          require "lsp_settings"
        end,
      }
      use {
        "j-hui/fidget.nvim",
        config = function()
          require("fidget").setup {
            text = {
              spinner = "meter", -- animation shown when tasks are ongoing
              done = "✔", -- character shown when all tasks are complete
              commenced = "Started", -- message shown when task starts
              completed = "Completed", -- message shown when task completes
            },
            align = {
              bottom = true, -- align fidgets along bottom edge of buffer
              right = true, -- align fidgets along right edge of buffer
            },
            timer = {
              spinner_rate = 125, -- frame rate of spinner animation, in ms
              fidget_decay = 2000, -- how long to keep around empty fidget, in ms
              task_decay = 1000, -- how long to keep around completed task, in ms
            },
            fmt = {
              leftpad = true, -- right-justify text in fidget box
              fidget = function(fidget_name, spinner)
                return string.format("%s %s", spinner, fidget_name)
              end,
              task = function(task_name, message, percentage)
                return string.format(
                  "%s%s [%s]",
                  message,
                  percentage and string.format(" (%s%%)", percentage) or "",
                  task_name
                )
              end,
            },
          }
        end,
      }
      use { "b0o/schemastore.nvim", requires = { { "neovim/nvim-lspconfig" } } }
      use {
        "jose-elias-alvarez/null-ls.nvim",
        requires = { { "neovim/nvim-lspconfig" }, { "nvim-lua/plenary.nvim" } },
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
                          vim.notify(
                            string.format("Added '%s'", w),
                            vim.log.levels.INFO,
                            { title = "[null-ls] cspell" }
                          )

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
              null_ls.builtins.diagnostics.flake8,
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
      }
      use {
        "hrsh7th/nvim-cmp",
        requires = {
          { "onsails/lspkind.nvim" },
          { "hrsh7th/vim-vsnip" },
          {
            "hrsh7th/cmp-vsnip",
            requires = "rafamadriz/friendly-snippets",
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
          -- {
          --   "L3MON4D3/LuaSnip",
          --   requires = "rafamadriz/friendly-snippets",
          --   config = function()
          --     require("luasnip.loaders.from_vscode").lazy_load()
          --     vim.cmd[[
          --       " press <Tab> to expand or jump in a snippet. These can also be mapped separately
          --       " via <Plug>luasnip-expand-snippet and <Plug>luasnip-jump-next.
          --       imap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>'
          --       smap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>'
          --       imap <silent><expr> <C-l> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<C-l>'
          --       smap <silent><expr> <C-l> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<C-l>'
          --       " -1 for jumping backwards.
          --       " inoremap <silent> <Tab> <cmd>lua require('luasnip').jump(1)<Cr>
          --       inoremap <silent> <S-Tab> <cmd>lua require'luasnip'.jump(-1)<Cr>
          --       snoremap <silent> <Tab> <cmd>lua require('luasnip').jump(1)<Cr>
          --       snoremap <silent> <S-Tab> <cmd>lua require('luasnip').jump(-1)<Cr>

          --       " For changing choices in choiceNodes (not strictly necessary for a basic setup).
          --       " imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
          --       " smap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
          --     ]]
          --   end,
          -- },
          { "saadparwaiz1/cmp_luasnip" },
          { "hrsh7th/cmp-buffer" },
          { "hrsh7th/cmp-path" },
          { "hrsh7th/cmp-cmdline" },
          {
            "petertriho/cmp-git",
            requires = "nvim-lua/plenary.nvim",
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
          { "hrsh7th/cmp-nvim-lsp-signature-help" },
          { "hrsh7th/cmp-nvim-lua" },
          { "windwp/nvim-autopairs" },
          { "lukas-reineke/cmp-rg", disable = true }, -- blocking completion
        },
        config = function()
          local cmp = require "cmp"
          local types = require "cmp.types"
          local compare = require "cmp.config.compare"
          local lspkind = require "lspkind"
          local cmp_autopairs = require "nvim-autopairs.completion.cmp"
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

            window = {
              completion = cmp.config.window.bordered(),
              documentation = cmp.config.window.bordered(),
            },

            -- You must set mapping.
            mapping = {
              ["<C-n>"] = cmp.mapping(
                cmp.mapping.select_next_item { behavior = types.cmp.SelectBehavior.Insert },
                { "i", "c" }
              ),
              ["<C-p>"] = cmp.mapping(
                cmp.mapping.select_prev_item { behavior = types.cmp.SelectBehavior.Insert },
                { "i", "c" }
              ),
              ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
              ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
              ["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "s", "c" }),
              ["<C-Space>"] = cmp.mapping.complete(),
              ["<C-e>"] = cmp.mapping {
                i = cmp.mapping.abort(),
                c = cmp.mapping.close(),
              },
              ["<CR>"] = cmp.mapping.confirm(),
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

          cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

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
      }
      use {
        "ray-x/lsp_signature.nvim",
        disable = true,
        config = function()
          local cfg = {
            debug = false, -- set to true to enable debug logging
            verbose = false, -- show debug line number
            -- If you want to hook lspsaga or other signature handler, pls set to false
            doc_lines = 10, -- will show two lines of comment/doc(if there are more than two lines in doc, will be truncated);
            -- set to 0 if you DO NOT want any API comments be shown
            -- This setting only take effect in insert mode, it does not affect signature help in normal
            -- mode, 10 by default

            floating_window = true, -- show hint in a floating window, set to false for virtual text only mode

            floating_window_above_cur_line = true, -- try to place the floating above the current line when possible Note:
            -- will set to true when fully tested, set to false will use whichever side has more space
            -- this setting will be helpful if you do not want the PUM and floating win overlap

            floating_window_off_x = 1, -- adjust float windows x position.
            floating_window_off_y = 1, -- adjust float windows y position.

            fix_pos = false, -- set to true, the floating window will not auto-close until finish all parameters
            hint_enable = false, -- virtual hint enable
            hint_prefix = " ", -- Panda for parameter
            max_height = 12, -- max height of signature floating_window, if content is more than max_height, you can scroll down
            -- to view the hiding contents
            max_width = 80, -- max_width of signature floating_window, line will be wrapped if exceed max_width
            handler_opts = {
              border = "rounded", -- double, rounded, single, shadow, none
            },

            always_trigger = false, -- sometime show signature on new line or in middle of parameter can be confusing, set it to false for #58

            auto_close_after = nil, -- autoclose signature float win after x sec, disabled if nil.
            extra_trigger_chars = {}, -- Array of extra characters that will trigger signature completion, e.g., {"(", ","}
            zindex = 200, -- by default it will be on top of all floating windows, set to <= 50 send it to bottom

            padding = "", -- character to pad on left and right of signature can be ' ', or '|'  etc

            transparency = 10, -- disabled by default, allow floating win transparent value 1~100
            shadow_blend = 36, -- if you using shadow as border use this set the opacity
            shadow_guibg = "Black", -- if you using shadow as border use this set the color e.g. 'Green' or '#121315'
            timer_interval = 200, -- default timer check interval set to lower value if you want to reduce latency
            toggle_key = nil, -- toggle signature on and off in insert mode,  e.g. toggle_key = '<M-x>'
          }

          require("lsp_signature").setup(cfg)
        end,
      }
      use {
        "folke/lsp-trouble.nvim",
        requires = "kyazdani42/nvim-web-devicons",
        config = function()
          require("trouble").setup {
            -- your configuration comes here
            -- or leave it empty to use the default settings
            -- refer to the configuration section below
            height = 10, -- height of the trouble list
            icons = true, -- use dev-icons for filenames
            mode = "document_diagnostics", -- "workspace" or "document"
            fold_open = "", -- icon used for open folds
            fold_closed = "", -- icon used for closed folds
            action_keys = { -- key mappings for actions in the trouble list
              close = "q", -- close the list
              refresh = "r", -- manually refresh
              jump = "<cr>", -- jump to the diagnostic or open / close folds
              toggle_mode = "m", -- toggle between "workspace" and "document" mode
              toggle_preview = "P", -- toggle auto_preview
              preview = "p", -- preview the diagnostic location
              close_folds = "zM", -- close all folds
              cancel = "<esc>", -- cancel the preview and get back to your last window / buffer / cursor
              open_folds = "zR", -- open all folds
              previous = "k", -- preview item
              next = "j", -- next item
            },
            indent_lines = true, -- add an indent guide below the fold icons
            auto_open = false, -- automatically open the list when you have diagnostics
            auto_close = false, -- automatically close the list when you have no diagnostics
            auto_preview = true, -- automatically preview the location of the diagnostic. <esc> to close preview and go back
            signs = {
              -- icons / text used for a diagnostic
              error = "",
              warning = "",
              hint = "",
              information = "",
            },
            use_lsp_diagnostic_signs = false, -- enabling this will use the signs defined in your lsp client
          }

          vim.api.nvim_set_keymap(
            "n",
            "<leader>sd",
            "<cmd>TroubleToggle document_diagnostics<cr>",
            require("scripts/util").keymaps.default_opt
          )
          vim.api.nvim_set_keymap(
            "n",
            "<leader>sD",
            "<cmd>TroubleToggle workspace_diagnostics<cr>",
            require("scripts/util").keymaps.default_opt
          )
        end,
      }

      use {
        "akinsho/flutter-tools.nvim",
        requires = { "neovim/nvim-lspconfig" },
        ft = { "dart" },
      }
    elseif vim.g.lsp_client_type == "coc" then
      use {
        "neoclide/coc.nvim",
        branch = "master",
        run = "yarn install --frozen-lockfile",
        requires = { "rafcamlet/coc-nvim-lua" },
      }
    end

    use {
      "folke/which-key.nvim",
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
          ["<leader>"] = {
            q = { "<cmd>Bdelete<CR>", "Delete Buffer" },
            Q = { "<cmd>Bdelete!<CR>", "Delete Buffer!" },
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
          ["<leader>s"] = {
            name = "+Search",
            b = {
              '<cmd>lua require("telescope.builtin").buffers{ show_all_buffers = true, generic_sorters = require("telescope.sorters").fuzzy_with_index_bias }<CR>',
              "Buffer",
            },
            c = {
              name = "+Commands",
              r = { "<cmd>lua require('telescope.builtin').command_history{}<CR>", "History" },
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
          },
          ["<leader>y"] = {
            name = "+Yode",
            c = { "<cmd>YodeCreateSeditorFloating<cr>", "Create Floating" },
            r = { "<cmd>YodeCreateSeditorReplace<cr>", "Replace" },
            d = { "<cmd>YodeBufferDelete<cr>", "Delete" },
            w = {
              name = "+Layout",
              d = { "<cmd>YodeLayoutShiftWinDown<cr>", "Layout Down" },
              u = { "<cmd>YodeLayoutShiftWinUp<cr>", "Layout Up" },
              j = { "<cmd>YodeLayoutShiftWinBottom<cr>", "Layout Bottom" },
              k = { "<cmd>YodeLayoutShiftWinTop<cr>", "Layout Top" },
            },
          },
          ["<leader>gl"] = {
            name = "+GitLinker",
            c = { "<cmd>GitLinker current<cr>", "Current git link" },
            d = { "<cmd>GitLinker default<cr>", "Default branch git link" },
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
          ["<leader>gl"] = {
            name = "+GitLinker",
            c = { "<cmd>'<,'>GitLinker current<cr>", "Current git link" },
            d = { "<cmd>'<,'>GitLinker default<cr>", "Default branch git link" },
          },
        }, { mode = "x" })

        if vim.g.lsp_client_type == "neovim" then
          wk.register({
            ["g"] = {
              name = "+LSP",
              r = { "<cmd>Telescope lsp_references<CR>", "References" },
              i = { "<cmd>Telescope lsp_implementations<CR>", "Implementations" },
              h = { "<cmd>Lspsaga lsp_finder<cr>", "Finder" },
              d = { "<cmd>Telescope lsp_definitions<CR>", "Definition" },
              D = { "<cmd>Telescope lsp_type_definitions<CR>", "Type Definition" },
              p = { "<cmd>Lspsaga preview_definition<CR>", "Preview Definition" },
            },
            ["K"] = { "<cmd>Lspsaga hover_doc<cr>", "Hover Doc" },
            ["rn"] = { "<cmd>Lspsaga rename<CR>", "Rename" },
            ["<leader>"] = {
              F = { "<cmd>lua vim.lsp.buf.format{async = true}<CR>", "Format" },
            },
            ["<leader>ac"] = { "<cmd>Lspsaga code_action<CR>", "Code Action" },
            ["<leader>d"] = {
              name = "+Diagnostics",
              c = { "<cmd>lua vim.diagnostic.open_float()<CR>", "Open Float" },
              o = { "<cmd>lua vim.diagnostic.setloclist()<CR>", "Set Loclist" },
              n = { "<cmd>Lspsaga diagnostic_jump_next<CR>", "Jump Next" },
              p = { "<cmd>Lspsaga diagnostic_jump_prev<CR>", "Jump Previous" },
            },
          }, { mode = "n" })
          wk.register({
            ["<leader>ac"] = {
              "<cmd><C-U>Lspsaga range_code_action<CR>",
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
    }

    use {
      -- "tkmpypy/chowcho.nvim",
      "~/ghq/github.com/tkmpypy/chowcho.nvim",
      config = function()
        require("chowcho").setup { border_style = "rounded", icon_enabled = true }
      end,
    }
    use {
      "~/ghq/github.com/tkmpypy/deepon.nvim",
      requires = {
        "MunifTanjim/nui.nvim",
        "nvim-lua/plenary.nvim",
      },
      config = function()
        require("deepon").setup()
      end,
    }
  end,
  config = {
    -- Move to lua dir so impatient.nvim can cache it
    compile_path = packer_compiled_path,
    display = { open_fn = packer_util.float },
  },
}
