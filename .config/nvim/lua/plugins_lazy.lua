---@diagnostic disable: missing-fields, undefined-global
local vim = vim
local lazypath = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy", "lazy.nvim")
if not vim.uv.fs_stat(lazypath) then
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
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    keys = {
      {
        "<leader>q",
        function()
          Snacks.bufdelete({})
        end,
        mode = "n",
        desc = "Delete buffer",
      },
      {
        "<leader>Q",
        function()
          Snacks.bufdelete({ force = true })
        end,
        mode = "n",
        desc = "Delete buffer and ignore changes",
      },
      {
        "<leader>glf",
        function()
          Snacks.lazygit.log_file()
        end,
        desc = "Lazygit Current File History",
      },
      {
        "<leader>gg",
        function()
          Snacks.lazygit()
        end,
        desc = "Lazygit",
      },
      {
        "<leader>gll",
        function()
          Snacks.lazygit.log()
        end,
        desc = "Lazygit Log (cwd)",
      },
      -- Top Pickers & Explorer
      -- {
      --   "<leader>e",
      --   function()
      --     Snacks.explorer({ hidden = true })
      --   end,
      --   desc = "File Explorer",
      -- },
      {
        "<leader><space>",
        function()
          Snacks.picker.smart()
        end,
        desc = "Smart Find Files",
      },
      {
        "<leader>,",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Buffers",
      },
      {
        "<leader>/",
        function()
          Snacks.picker.grep({ hidden = true })
        end,
        desc = "Grep",
      },
      {
        "<leader>:",
        function()
          Snacks.picker.command_history()
        end,
        desc = "Command History",
      },
      {
        "<leader>n",
        function()
          Snacks.picker.notifications()
        end,
        desc = "Notification History",
      },
      -- find
      {
        "<leader>fb",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Buffers",
      },
      {
        "<leader>fc",
        function()
          Snacks.picker.files({ cwd = vim.fn.stdpath("config"), hidden = true })
        end,
        desc = "Find Config File",
      },
      {
        "<leader>ff",
        function()
          Snacks.picker.files({ hidden = true })
        end,
        desc = "Find Files",
      },
      {
        "<leader>fg",
        function()
          Snacks.picker.git_files()
        end,
        desc = "Find Git Files",
      },
      {
        "<leader>fr",
        function()
          Snacks.picker.recent()
        end,
        desc = "Recent",
      },
      -- git
      {
        "<leader>gb",
        function()
          Snacks.picker.git_branches()
        end,
        desc = "Git Branches",
      },
      {
        "<leader>gl",
        function()
          Snacks.picker.git_log()
        end,
        desc = "Git Log",
      },
      {
        "<leader>gL",
        function()
          Snacks.picker.git_log_line()
        end,
        desc = "Git Log Line",
      },
      {
        "<leader>gs",
        function()
          Snacks.picker.git_status()
        end,
        desc = "Git Status",
      },
      {
        "<leader>gS",
        function()
          Snacks.picker.git_stash()
        end,
        desc = "Git Stash",
      },
      {
        "<leader>gd",
        function()
          Snacks.picker.git_diff()
        end,
        desc = "Git Diff (Hunks)",
      },
      {
        "<leader>gf",
        function()
          Snacks.picker.git_log_file()
        end,
        desc = "Git Log File",
      },
      {
        "<leader>gB",
        function()
          Snacks.gitbrowse()
        end,
        desc = "Git Browse",
        mode = { "n", "v" },
      },
      -- Grep
      {
        "<leader>sb",
        function()
          Snacks.picker.lines()
        end,
        desc = "Buffer Lines",
      },
      {
        "<leader>sB",
        function()
          Snacks.picker.grep_buffers()
        end,
        desc = "Grep Open Buffers",
      },
      {
        "<leader>sg",
        function()
          Snacks.picker.grep({ hidden = true })
        end,
        desc = "Grep",
      },
      {
        "<leader>sw",
        function()
          Snacks.picker.grep_word({ hidden = true })
        end,
        desc = "Visual selection or word",
        mode = { "n", "x" },
      },
      -- search
      {
        '<leader>s"',
        function()
          Snacks.picker.registers()
        end,
        desc = "Registers",
      },
      {
        "<leader>s/",
        function()
          Snacks.picker.search_history()
        end,
        desc = "Search History",
      },
      {
        "<leader>sa",
        function()
          Snacks.picker.autocmds()
        end,
        desc = "Autocmds",
      },
      {
        "<leader>sb",
        function()
          Snacks.picker.lines()
        end,
        desc = "Buffer Lines",
      },
      {
        "<leader>sc",
        function()
          Snacks.picker.command_history()
        end,
        desc = "Command History",
      },
      {
        "<leader>sC",
        function()
          Snacks.picker.commands()
        end,
        desc = "Commands",
      },
      {
        "<leader>sd",
        function()
          Snacks.picker.diagnostics()
        end,
        desc = "Diagnostics",
      },
      {
        "<leader>sD",
        function()
          Snacks.picker.diagnostics_buffer()
        end,
        desc = "Buffer Diagnostics",
      },
      {
        "<leader>sh",
        function()
          Snacks.picker.help()
        end,
        desc = "Help Pages",
      },
      {
        "<leader>sH",
        function()
          Snacks.picker.highlights()
        end,
        desc = "Highlights",
      },
      {
        "<leader>si",
        function()
          Snacks.picker.icons()
        end,
        desc = "Icons",
      },
      {
        "<leader>sj",
        function()
          Snacks.picker.jumps()
        end,
        desc = "Jumps",
      },
      {
        "<leader>sk",
        function()
          Snacks.picker.keymaps()
        end,
        desc = "Keymaps",
      },
      {
        "<leader>sl",
        function()
          Snacks.picker.loclist()
        end,
        desc = "Location List",
      },
      {
        "<leader>sm",
        function()
          Snacks.picker.marks()
        end,
        desc = "Marks",
      },
      {
        "<leader>sM",
        function()
          Snacks.picker.man()
        end,
        desc = "Man Pages",
      },
      {
        "<leader>sp",
        function()
          Snacks.picker.lazy()
        end,
        desc = "Search for Plugin Spec",
      },
      {
        "<leader>sq",
        function()
          Snacks.picker.qflist()
        end,
        desc = "Quickfix List",
      },
      {
        "<leader>sR",
        function()
          Snacks.picker.resume()
        end,
        desc = "Resume",
      },
      {
        "<leader>su",
        function()
          Snacks.picker.undo()
        end,
        desc = "Undo History",
      },
      {
        "<leader>uC",
        function()
          Snacks.picker.colorschemes()
        end,
        desc = "Colorschemes",
      },
      -- LSP
      {
        "gd",
        function()
          Snacks.picker.lsp_definitions()
        end,
        desc = "Goto Definition",
      },
      {
        "gD",
        function()
          Snacks.picker.lsp_declarations()
        end,
        desc = "Goto Declaration",
      },
      {
        "gr",
        function()
          Snacks.picker.lsp_references()
        end,
        nowait = true,
        desc = "References",
      },
      {
        "gI",
        function()
          Snacks.picker.lsp_implementations()
        end,
        desc = "Goto Implementation",
      },
      {
        "gy",
        function()
          Snacks.picker.lsp_type_definitions()
        end,
        desc = "Goto T[y]pe Definition",
      },
      {
        "<leader>ss",
        function()
          Snacks.picker.lsp_symbols()
        end,
        desc = "LSP Symbols",
      },
      {
        "<leader>sS",
        function()
          Snacks.picker.lsp_workspace_symbols()
        end,
        desc = "LSP Workspace Symbols",
      },
    },
    ---@type snacks.Config
    opts = {
      picker = {
        -- sources = {
        --   select = {
        --     layout = {
        --       preset = "telescope",
        --     },
        --   },
        -- },
        formatters = {
          file = {
            truncate = 100,
          },
        },
      },
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
      bigfile = {
        notify = true, -- show notification when big file detected
        size = 1.5 * 1024 * 1024, -- 1.5MB
      },
      bufdelete = { enabled = true },
      dashboard = {
        sections = {
          { section = "header" },
          { section = "keys", gap = 1, padding = 1 },
          {
            pane = 2,
            icon = " ",
            desc = "Browse Repo",
            padding = 1,
            key = "b",
            action = function()
              Snacks.gitbrowse()
            end,
          },
          function()
            local in_git = Snacks.git.get_root() ~= nil
            local cmds = {
              {
                title = "Open Issues",
                cmd = "gh issue list -L 3",
                key = "i",
                action = function()
                  vim.fn.jobstart("gh issue list --web", { detach = true })
                end,
                icon = " ",
                height = 7,
              },
              {
                icon = " ",
                title = "Open PRs",
                cmd = "gh pr list -L 3",
                key = "p",
                action = function()
                  vim.fn.jobstart("gh pr list --web", { detach = true })
                end,
                height = 7,
              },
              {
                icon = " ",
                title = "Git Status",
                cmd = "git --no-pager diff --stat -B -M -C",
                height = 10,
              },
            }
            return vim.tbl_map(function(cmd)
              return vim.tbl_extend("force", {
                pane = 2,
                section = "terminal",
                enabled = in_git,
                padding = 1,
                ttl = 5 * 60,
                indent = 3,
              }, cmd)
            end, cmds)
          end,
          { section = "startup" },
        },
      },
      indent = {
        indent = { enabled = true },
        scope = { enabled = true },
      },
      lazygit = {
        configure = true,
      },
      input = {
        icon = " ",
        icon_hl = "SnacksInputIcon",
        icon_pos = "left",
        prompt_pos = "title",
        win = { style = "input" },
        expand = true,
      },
      image = {
        formats = {},
      },
    },
  },
  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    enabled = function()
      return vim.g.use_treesitter
    end,
    build = ":TSUpdate",
    dependencies = {
      "RRethy/nvim-treesitter-endwise",
      "windwp/nvim-ts-autotag",
    },
    config = function()
      require("nvim-treesitter.configs").setup({
        highlight = {
          enable = true,
          disable = function(lang, buf)
            local disabled_lang = { "org" }
            if vim.tbl_contains(disabled_lang, lang) then
              return true
            end

            local max_filesize = 200 * 1024 -- 200 KB
            local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
              return true
            end
            return false
          end,
        },
        indent = {
          enable = true,
        },
        -- Install parsers synchronously (only applied to `ensure_installed`)
        sync_install = false,

        -- Automatically install missing parsers when entering buffer
        auto_install = false,
        ensure_installed = {
          "bash",
          "java",
          "dart",
          "go",
          "gomod",
          "gosum",
          "gowork",
          "rust",
          "ruby",
          "python",
          "lua",
          "luadoc",
          "php",
          "php_only",
          "phpdoc",
          "blade",
          "phpdoc",
          "yaml",
          "toml",
          "json",
          "jsonnet",
          "typescript",
          "javascript",
          "jsdoc",
          "tsx",
          "html",
          "haskell",
          "haskell_persistent",
          "vim",
          "vimdoc",
          "markdown",
          "markdown_inline",
          "mermaid",
          "regex",
          "make",
          "dockerfile",
          "graphql",
          "sql",
          "terraform",
          "proto",
          "diff",
          "comment",
          "git_config",
          "git_rebase",
          "gitattributes",
          "gitcommit",
          "gitignore",
          "ini",
        },
        endwise = {
          enable = true,
        },
        autotag = {
          enable = true,
        },
      })
    end,
  },
  {
    "kevinhwang91/nvim-ufo",
    dependencies = {
      "kevinhwang91/promise-async",
      "neovim/nvim-lspconfig",
      "nvim-treesitter/nvim-treesitter",
      {
        "luukvbaal/statuscol.nvim",
        config = function()
          local builtin = require("statuscol.builtin")
          require("statuscol").setup({
            bt_ignore = { "terminal", "nofile" },
            ft_ignore = { "alpha", "NeogitStatus", "NeogitCommitMessage" },
            -- configuration goes here, for example:
            relculright = true,
            segments = {
              {
                sign = {
                  namespace = { "gitsigns" },
                  maxwidth = 1,
                  colwidth = 1,
                  wrap = true,
                },
              },
              {
                sign = { namespace = { "diagnostic/signs" }, maxwidth = 1, auto = true },
                click = "v:lua.ScSa",
              },
              { text = { builtin.foldfunc, " " }, click = "v:lua.ScFa" },
              { text = { builtin.lnumfunc, " " }, click = "v:lua.ScLa" },
            },
          })
        end,
      },
    },
    event = { "VeryLazy" },
    config = function()
      local ftMap = {
        vim = "indent",
        python = { "indent" },
        git = "",
      }
      require("ufo").setup({
        open_fold_hl_timeout = 150,
        -- provider_selector = function(bufnr, filetype, buftype)
        --   -- if you prefer treesitter provider rather than lsp,
        --   return ftMap[filetype] or { "treesitter", "indent" }
        --
        --   -- refer to ./doc/example.lua for detail
        -- end,
      })
    end,
    keys = {
      {
        "zR",
        function()
          return require("ufo").openAllFolds()
        end,
        mode = { "n" },
        noremap = true,
        silent = true,
        expr = true,
      },
      {
        "zM",
        function()
          return require("ufo").closeAllFolds()
        end,
        mode = { "n" },
        noremap = true,
        silent = true,
        expr = true,
      },
      {
        "zr",
        function()
          return require("ufo").openFoldsExceptKinds()
        end,
        mode = { "n" },
        noremap = true,
        silent = true,
        expr = true,
      },
      {
        "zm",
        function()
          return require("ufo").closeFoldsWith()
        end,
        mode = { "n" },
        noremap = true,
        silent = true,
        expr = true,
      },
    },
  },
  {
    "HiPhish/rainbow-delimiters.nvim",
    event = "BufReadPre",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      local rainbow_delimiters = require("rainbow-delimiters")
      require("rainbow-delimiters.setup").setup({
        strategy = {
          [""] = rainbow_delimiters.strategy["global"],
          vim = rainbow_delimiters.strategy["local"],
        },
        query = {
          [""] = "rainbow-delimiters",
          lua = "rainbow-blocks",
        },
        highlight = {
          "RainbowDelimiterRed",
          "RainbowDelimiterYellow",
          "RainbowDelimiterBlue",
          "RainbowDelimiterOrange",
          "RainbowDelimiterGreen",
          "RainbowDelimiterViolet",
          "RainbowDelimiterCyan",
        },
      })
    end,
  },
  {
    "danymat/neogen",
    lazy = true,
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "L3MON4D3/LuaSnip",
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
  -- runner
  {
    "stevearc/overseer.nvim",
    cmd = {
      "OverseerOpen",
      "OverseerClose",
      "OverseerToggle",
      "OverseerSaveBundle",
      "OverseerLoadBundle",
      "OverseerDeleteBundle",
      "OverseerRunCmd",
      "OverseerRun",
      "OverseerInfo",
      "OverseerBuild",
      "OverseerQuickAction",
      "OverseerTaskAction",
      "OverseerClearCache",
      "OverseerRestartLast",
    },
    config = function()
      require("overseer").setup()
      vim.api.nvim_create_user_command("OverseerRestartLast", function()
        local overseer = require("overseer")
        local tasks = overseer.list_tasks({ recent_first = true })
        if vim.tbl_isempty(tasks) then
          vim.notify("No tasks found", vim.log.levels.WARN)
        else
          overseer.run_action(tasks[1], "restart")
        end
      end, {})
    end,
  },
  {
    "michaelb/sniprun",
    build = "sh install.sh",
    cmd = { "SnipRun", "SnipInfo", "SnipReset", "SnipReplMemoryClean", "SnipClose", "SnipLive" },
    config = function()
      require("sniprun").setup({
        selected_interpreters = {
          -- "Python3_original",
          "Lua_nvim",
          "Go_original",
          "Generic",
        }, --# use those instead of the default for the current filetype
        -- repl_enable = {}, --# enable REPL-like behavior for the given interpreters
        -- repl_disable = {}, --# disable REPL-like behavior for the given interpreters

        interpreter_options = { --# interpreter-specific options, see doc / :SnipInfo <name>

          --# use the interpreter name as key
          GFM_original = {
            use_on_filetypes = { "markdown.pandoc" }, --# the 'use_on_filetypes' configuration key is
            --# available for every interpreter
          },
          Python3_original = {
            error_truncate = "auto", --# Truncate runtime errors 'long', 'short' or 'auto'
            --# the hint is available for every interpreter
            --# but may not be always respected
          },
          Generic = {
            error_truncate = "long", -- strongly recommended to figure out what's going on
            PHP = { -- any key name is ok
              supported_filetypes = { "php" }, -- mandatory
              extension = ".php", -- recommended, but not mandatory. Sniprun use this to create temporary files
              boilerplate_pre = "<?php",
              interpreter = "php", -- interpreter or compiler (+ options if any)
              compiler = "", -- one of those MUST be non-empty
            },
          },
        },

        --# you can combo different display modes as desired and with the 'Ok' or 'Err' suffix
        --# to filter only sucessful runs (or errored-out runs respectively)
        display = {
          -- "Classic", --# display results in the command-line  area
          -- "VirtualTextOk", --# display ok results as virtual text (multiline is shortened)

          -- "VirtualText",             --# display results as virtual text
          -- "TempFloatingWindow",      --# display results in a floating window
          -- "LongTempFloatingWindow",  --# same as above, but only long results. To use with VirtualText[Ok/Err]
          "Terminal", --# display results in a vertical split
          -- "TerminalWithCode",        --# display results and code history in a vertical split
          -- "NvimNotify", --# display with the nvim-notify plugin
          -- "Api"                      --# return output to a programming interface
        },

        live_display = { "VirtualText", "TerminalOk" }, --..or anything you want

        -- display_options = {
        --   terminal_scrollback = vim.o.scrollback, --# change terminal display scrollback lines
        --   terminal_line_number = false, --# whether show line number in terminal window
        --   terminal_signcolumn = false, --# whether show signcolumn in terminal window
        --   terminal_persistence = true, --# always keep the terminal open (true) or close it at every occasion (false)
        --   terminal_width = 45, --# change the terminal display option width
        --   notification_timeout = 5, --# timeout for nvim_notify output
        -- },

        --# You can use the same keys to customize whether a sniprun producing
        --# no output should display nothing or '(no output)'
        -- show_no_output = {
        --   "Classic",
        --   "TempFloatingWindow", --# implies LongTempFloatingWindow, which has no effect on its own
        -- },

        --# customize highlight groups (setting this overrides colorscheme)
        snipruncolors = {
          SniprunVirtualTextOk = { bg = "#66eeff", fg = "#000000", ctermbg = "Cyan", ctermfg = "Black" },
          SniprunFloatingWinOk = { fg = "#66eeff", ctermfg = "Cyan" },
          SniprunVirtualTextErr = { bg = "#881515", fg = "#000000", ctermbg = "DarkRed", ctermfg = "Black" },
          SniprunFloatingWinErr = { fg = "#881515", ctermfg = "DarkRed" },
        },

        -- live_mode_toggle = "off", --# live mode toggle, see Usage - Running for more info

        --# miscellaneous compatibility/adjustement settings
        inline_messages = false, --# boolean toggle for a one-line way to display messages
        --# to workaround sniprun not being able to display anything

        borders = "single", --# display borders around floating windows
        --# possible values are 'none', 'single', 'double', or 'shadow'
      })
    end,
  },
  {
    "numToStr/Comment.nvim",
    opts = true,
    lazy = false,
  },
  {
    "nvim-neotest/neotest",
    lazy = true,
    enabled = function()
      return vim.g.test_runner_type == "neotest"
    end,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-neotest/nvim-nio",
      "nvim-neotest/neotest-go",
      "nvim-neotest/neotest-python",
      "olimorris/neotest-phpunit",
      -- "praem90/neotest-docker-phpunit.nvim",
      "rouge8/neotest-rust",
    },
    config = function()
      local neotest_ns = vim.api.nvim_create_namespace("neotest")
      vim.diagnostic.config({
        virtual_text = {
          format = function(diagnostic)
            local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
            return message
          end,
        },
      }, neotest_ns)

      local group = vim.api.nvim_create_augroup("NeotestConfig", {})
      for _, ft in ipairs({ "output", "output-panel", "attach", "summary" }) do
        vim.api.nvim_create_autocmd("FileType", {
          pattern = "neotest-" .. ft,
          group = group,
          callback = function(opts)
            vim.keymap.set("n", "q", function()
              pcall(vim.api.nvim_win_close, 0, true)
            end, {
              buffer = opts.buf,
            })
          end,
        })
      end
      require("neotest").setup({
        -- your neotest config here
        adapters = {
          require("neotest-go")({

            experimental = {
              test_table = true,
            },
            args = { "-v" },
          }),
          require("neotest-rust")({
            args = { "--no-capture" },
          }),
          require("neotest-python")({
            -- Extra arguments for nvim-dap configuration
            -- See https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings for values
            dap = { justMyCode = false },
            -- Command line arguments for runner
            -- Can also be a function to return dynamic values
            args = { "-vv", "--capture=no", "--log-level", "DEBUG" },
            runner = "pytest",
            pytest_discover_instances = true,
            python = function()
              return vim.fn.trim(vim.fn.system({ "which", "python" }))
            end,
            -- Returns if a given file path is a test file.
            -- NB: This function is called a lot so don't perform any heavy tasks within it.
            -- is_test_file = function(file_path)
            --   local s, e = file_path:find("test", 1, true)
            --   local b = s ~= nil
            --   print(b)

            --   return b
            -- end,
          }),
          require("neotest-phpunit")({
            phpunit_cmd = function()
              return "vendor/bin/phpunit"
            end,
            filter_dirs = { "vendor" },
          }),
        },
        benchmark = {
          enabled = true,
        },
        default_strategy = "integrated",
        diagnostic = {
          enabled = true,
          severity = 1,
        },
        discovery = {
          concurrent = 5,
          enabled = true,
        },
        jump = {
          enabled = true,
        },
        output = {
          enabled = true,
          open_on_run = true,
        },
        output_panel = {
          enabled = true,
          open = "botright split | resize 15",
        },
        quickfix = {
          enabled = false,
          open = true,
        },
        run = {
          enabled = true,
        },
        running = {
          concurrent = true,
        },
        state = {
          enabled = true,
        },
        status = {
          enabled = true,
          signs = false,
          virtual_text = true,
        },
        strategies = {
          integrated = {
            height = 40,
            width = 120,
          },
        },
        summary = {
          animated = true,
          enabled = true,
          expand_errors = true,
          follow = true,
          mappings = {
            attach = "a",
            clear_marked = "M",
            clear_target = "T",
            debug = "d",
            debug_marked = "D",
            expand = { "<CR>", "<2-LeftMouse>" },
            expand_all = "e",
            jumpto = "i",
            mark = "m",
            next_failed = "J",
            output = "o",
            prev_failed = "K",
            run = "r",
            run_marked = "R",
            short = "O",
            stop = "u",
            target = "t",
          },
          open = "botright vsplit | vertical resize 50",
        },
      })
    end,
  },
  {
    "vim-test/vim-test",
    enabled = function()
      return vim.g.test_runner_type == "vim-test"
    end,
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
    enabled = false,
  },
  {
    "echasnovski/mini.icons",
    version = "*",
    opts = {
      style = "glyph", -- or 'glyph'
    },
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
    event = "WinNew",
    enabled = false,
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
        smooth = true,
        create_event = function() end,
        close_event = function() end,
      })
    end,
  },
  {
    "petertriho/nvim-scrollbar",
    event = { "VeryLazy" },
    config = function()
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
          search = false, -- Requires hlslens to be loaded, will run require("scrollbar.handlers.search").setup() for you
        },
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
      local lint_progress = function()
        local linters = require("lint").get_running()
        if #linters == 0 then
          return " "
        end
        return "󰔟 " .. table.concat(linters, ", ")
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
          disabled_filetypes = {
            statusline = {
              "TelescopePrompt",
            },
            winbar = {},
          },
          always_divide_middle = true,
          globalstatus = true,
          refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
          },
        },
        sections = {
          lualine_a = { "mode" },
          lualine_b = {
            {
              "filetype",
              icon_only = false,
              icon = { align = "right" },
            },
            {
              "filename",
              symbols = { modified = " ", readonly = "  ", unnamed = "  " },
              path = 1,
            },
          },
          lualine_c = {
            {
              lsp_status(),
              icon = "󰒋",
              color = {
                fg = theme.normal.a.bg,
                gui = "bold",
              },
            },
            {
              "diagnostics",
              always_visible = true,
              symbols = { error = " ", warn = " ", info = " ", hint = " " },
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
            {
              lint_progress,
              color = {
                fg = lualine_utils.extract_color_from_hllist("fg", {
                  "SpecialComment",
                  "Comment",
                }, "#ffffff"),
                gui = "bold",
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
              symbols = { added = " ", modified = " ", removed = " " },
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
          enabled = true,
          view = "cmdline", -- view for rendering the cmdline. Change to `cmdline` to get a classic cmdline at the bottom
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
            enabled = false,
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
            silent = true,
          },
          signature = {
            enabled = false,
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
    "stevearc/oil.nvim",
    opts = {
      git = {
        -- Return true to automatically git add/mv/rm files
        add = function(path)
          return true
        end,
        mv = function(src_path, dest_path)
          return true
        end,
        rm = function(path)
          return true
        end,
      },
      keymaps = {
        ["gd"] = {
          desc = "Toggle file detail view",
          callback = function()
            OIL_DETAIL = not OIL_DETAIL
            if OIL_DETAIL then
              require("oil").set_columns({ "icon", "permissions", "size", "mtime" })
            else
              require("oil").set_columns({ "icon" })
            end
          end,
        },
        ["gv"] = { "actions.select", opts = { vertical = true }, desc = "Open the entry in a vertical split" },
        ["gs"] = { "actions.select", opts = { horizontal = true }, desc = "Open the entry in a horizontal split" },
      },
      columns = {
        "icon",
        -- "permissions",
        -- "size",
        -- "mtime",
      },
      delete_to_trash = true,
      view_options = {
        show_hidden = true,
      },
      float = {
        -- Padding around the floating window
        padding = 10,
        max_width = 0,
        max_height = 0,
        border = "rounded",
        win_options = {
          winblend = 2,
        },
      },
    },
    -- Optional dependencies
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    cmd = { "Neotree" },
    enabled = function()
      return vim.g.file_explorer_type == "neo-tree"
    end,
    dependencies = {
      "nvim-lua/plenary.nvim",
      -- "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "echasnovski/mini.icons",
      "MunifTanjim/nui.nvim",
      {
        -- only needed if you want to use the commands with "_with_window_picker" suffix
        "s1n7ax/nvim-window-picker",
        version = "2.*",
        config = function()
          require("window-picker").setup({
            -- hint = "floating-big-letter",
            filter_rules = {
              include_current_win = false,
              autoselect_one = true,
              -- filter using buffer options
              bo = {
                -- if the file type is one of following, the window will be ignored
                filetype = { "neo-tree", "neo-tree-popup", "notify", "noice" },
                -- if the buffer type is one of following, the window will be ignored
                buftype = { "terminal", "quickfix" },
              },
            },
            show_prompt = false,
          })
        end,
      },
    },
    config = function()
      -- Unless you are still migrating, remove the deprecated commands from v1.x
      vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])

      require("neo-tree").setup({
        git_status_async = true,
        -- These options are for people with VERY large git repos
        git_status_async_options = {
          batch_size = 1000, -- how many lines of git status results to process at a time
          batch_delay = 10, -- delay in ms between batches. Spreads out the workload to let other processes run.
          max_lines = 10000, -- How many lines of git status results to process. Anything after this will be dropped.
          -- Anything before this will be used. The last items to be processed are the untracked files.
        },
        source_selector = {
          winbar = true,
          statusline = true,
          sources = {
            { source = "filesystem", display_name = " 󰉓 Files " },
            { source = "git_status", display_name = " 󰊢 Git " },
            { source = "buffers", display_name = "  Buffers " },
          },
        },
        document_symbols = {
          kinds = {
            File = { icon = "󰈙", hl = "Tag" },
            Namespace = { icon = "󰌗", hl = "Include" },
            Package = { icon = "󰏖", hl = "Label" },
            Class = { icon = "󰌗", hl = "Include" },
            Property = { icon = "󰆧", hl = "@property" },
            Enum = { icon = "󰒻", hl = "@number" },
            Function = { icon = "󰊕", hl = "Function" },
            String = { icon = "󰀬", hl = "String" },
            Number = { icon = "󰎠", hl = "Number" },
            Array = { icon = "󰅪", hl = "Type" },
            Object = { icon = "󰅩", hl = "Type" },
            Key = { icon = "󰌋", hl = "" },
            Struct = { icon = "󰌗", hl = "Type" },
            Operator = { icon = "󰆕", hl = "Operator" },
            TypeParameter = { icon = "󰊄", hl = "Type" },
            StaticMethod = { icon = "󰠄 ", hl = "Function" },
          },
        },
        close_if_last_window = false, -- Close Neo-tree if it is the last window left in the tab
        popup_border_style = "single",
        enable_git_status = true,
        enable_diagnostics = false,
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
            provider = function(icon, node)
              local text, hl
              local mini_icons = require("mini.icons")
              if node.type == "file" then
                text, hl = mini_icons.get("file", node.name)
              elseif node.type == "directory" then
                text, hl = mini_icons.get("directory", node.name)
                if node:is_expanded() then
                  text = nil
                end
              end

              if text then
                icon.text = text
              end
              if hl then
                icon.highlight = hl
              end
            end,
          },
          kind_icon = {
            provider = function(icon, node)
              icon.text, icon.highlight = require("mini.icons").get("lsp", node.extra.kind.name)
            end,
          },
          modified = {
            symbol = " ",
            highlight = "NeoTreeModified",
          },
          name = {
            trailing_slash = false,
            use_git_status_colors = true,
            highlight = "NeoTreeFileName",
          },
          symlink_target = {
            enabled = false,
          },
          git_status = {
            symbols = {
              -- Change type
              added = "", -- or "✚", but this is redundant info if you use git_status_colors on the name
              modified = "", -- or "", but this is redundant info if you use git_status_colors on the name
              deleted = "", -- this can only be used in the git_status source
              renamed = "󰁕", -- this can only be used in the git_status source
              -- Status type
              untracked = "",
              ignored = "󱋯",
              unstaged = "󰄱",
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
            ["o"] = "open",
            ["P"] = { "toggle_preview", config = { use_float = true } },
            ["l"] = "focus_preview",
            -- ["<C-x>"] = "open_split",
            -- ["<C-v>"] = "open_vsplit",
            ["<C-s>"] = "split_with_window_picker",
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
          follow_current_file = {
            enabled = false,
          },
          -- time the current file is changed while the tree is open.
          group_empty_dirs = false, -- when true, empty folders will be grouped together
          -- hijack_netrw_behavior = "disabled", -- netrw disabled, opening a directory opens neo-tree
          -- in whatever position is specified in window.position
          -- "open_current",  -- netrw disabled, opening a directory opens within the
          -- window like netrw would, regardless of window.position
          -- "disabled",    -- netrw left alone, neo-tree does not handle opening dirs
          use_libuv_file_watcher = false, -- This will use the OS level file watchers to detect changes
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
          follow_current_file = {
            enabled = false,
          },
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

  -- Lua Utils
  {
    "rafcamlet/nvim-luapad",
    cmd = "Luapad",
  },

  -- Operator
  {
    "kana/vim-operator-replace",
    event = { "VeryLazy" },
    dependencies = { "kana/vim-operator-user" },
  },

  -- Utils
  {
    "uga-rosa/translate.nvim",
    cmd = { "Translate" },
    config = function()
      local api_key = os.getenv("DEEPL_API_KEY")
      if api_key == nil then
        vim.notify_once("'DEEPL_API_KEY' is not set", vim.log.levels.WARN)
      end
      vim.g.deepl_api_auth_key = api_key

      local group = vim.api.nvim_create_augroup("TranslateGroup", {})
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "translate",
        group = group,
        callback = function(opts)
          vim.keymap.set("n", "q", function()
            pcall(vim.api.nvim_win_close, 0, true)
          end, {
            buffer = opts.buf,
          })
        end,
      })

      require("translate").setup({
        default = {
          command = "deepl_free",
          output = "split",
        },
        preset = {
          output = {
            split = {
              position = "top",
              min_size = 5,
              max_size = 0.8,
              append = false,
            },
          },
        },
      })
    end,
  },
  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    ft = { "org" },
    enabled = false,
    config = function()
      -- Setup orgmode
      local base_dir = "~/Google Drive/マイドライブ"
      require("orgmode").setup({
        ui = {
          menu = {
            handler = function(data)
              -- your handler here, for example:
              local options = {}
              local options_by_label = {}

              for _, item in ipairs(data.items) do
                -- Only MenuOption has `key`
                -- Also we don't need `Quit` option because we can close the menu with ESC
                if item.key and item.label:lower() ~= "quit" then
                  table.insert(options, item.label)
                  options_by_label[item.label] = item
                end
              end

              local handler = function(choice)
                if not choice then
                  return
                end

                local option = options_by_label[choice]
                if option.action then
                  option.action()
                end
              end

              vim.ui.select(options, {
                prompt = data.prompt,
              }, handler)
            end,
          },
        },
        mappings = {
          org_return_uses_meta_return = false,
        },
        org_startup_folded = "showeverything",
        org_id_link_to_org_use_id = false,
        org_agenda_files = base_dir .. "/org/*",
        org_default_notes_file = base_dir .. "/org/note.org",
        org_default_journal_file = base_dir .. "/org/journal.org",
        org_todo_keywords = { "TODO", "DOING", "WAITING", "|", "DONE", "DELEGATED" },
        win_split_mode = "auto",
        win_border = "single",
        org_archive_location = base_dir .. "/org/archive/%s::",
        org_capture_templates = {
          t = {
            description = "Todo",
            -- * TODO
            --   - [ ] Implements
            --   - [ ] User Review
            --   - [ ] Code Review
            --   - [ ] Release
            template = "* TODO %?\n \t- [ ] Implements\n\t- [ ] User Review\n\t- [ ] Code Review\n\t- [ ] Release",
            target = base_dir .. "/org/todo.org",
          },
          j = {
            description = "Journal",
            template = "\n*** %<%Y-%m-%d> %<%A>\n**** %U\n\n%?",
            target = base_dir .. "/org/journal.org",
          },
          b = {
            description = "BIZ Note",
            template = "** %?",
            target = base_dir .. "/org/note_biz.org",
          },
          d = {
            description = "DEV Note",
            template = "** %?",
            target = base_dir .. "/org/note_dev.org",
          },
        },
      })

      local cmd_tmpl = ":e " .. base_dir .. "/org/%s.org<CR>"
      vim.keymap.set("n", "<Leader>Ot", cmd_tmpl:format("todo"))
      vim.keymap.set("n", "<Leader>On", cmd_tmpl:format("note"))
      vim.keymap.set("n", "<Leader>Ob", cmd_tmpl:format("note_biz"))
      vim.keymap.set("n", "<Leader>Od", cmd_tmpl:format("note_dev"))
      vim.keymap.set("n", "<Leader>Oj", cmd_tmpl:format("journal"))
    end,
  },
  {
    "akinsho/org-bullets.nvim",
    ft = { "org" },
    config = function()
      require("org-bullets").setup({
        concealcursor = false, -- If false then when the cursor is on a line underlying characters are visible
        symbols = {
          -- list symbol
          list = "•",
          -- headlines can be a list
          headlines = { "◉ ", "○ ", "✸ ", "✿ " },
          -- or a function that receives the defaults and returns a list
          checkboxes = {
            half = { "", "OrgTSCheckboxHalfChecked" },
            done = { "✓", "OrgDone" },
            todo = { "˟", "OrgTODO" },
          },
        },
      })
    end,
  },
  {
    "epwalsh/obsidian.nvim",
    lazy = true,
    ft = "markdown",
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
    -- event = {
    --   -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
    --   -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/**.md"
    --   string.format("BufReadPre %s/%s", vim.fn.expand("~"), "Dropbox/notes/**.md"),
    --   string.format("BufNewFile %s/%s", vim.fn.expand("~"), "Dropbox/notes/**.md"),
    -- },
    dependencies = {
      -- Required.
      "nvim-lua/plenary.nvim",
    },
    config = function()
      local enable_cmp = false
      if vim.g.lsp_client_type == "neovim" and vim.g.complete_engine_type == "cmp" then
        enable_cmp = true
      end
      require("obsidian").setup({
        dir = "~/ghq/github.com/tkmpypy/obsidian",
        daily_notes = {
          folder = "journal",
        },
        completion = {
          nvim_cmp = enable_cmp, -- if using nvim-cmp, otherwise set to false
        },
        -- Optional, key mappings.
        mappings = {
          -- Overrides the 'gf' mapping to work on markdown/wiki links within your vault.
          -- ["ol"] = require("obsidian.mapping").gf_passthrough(),
        },

        note_id_func = function(title)
          -- Create note IDs in a Zettelkasten format with a timestamp and a suffix.
          local suffix = ""
          if title ~= nil then
            -- If title is given, transform it into valid file name.
            suffix = title:gsub(" ", "-"):gsub("[^A-Za-z0-9-ぁ-んァ-ヶー一-龯]", ""):lower()
          else
            -- If title is nil, just add 4 random uppercase letters to the suffix.
            for _ = 1, 4 do
              suffix = suffix .. string.char(math.random(65, 90))
            end
          end
          return tostring(os.date("%Y-%m-%d")) .. "-" .. suffix
        end,
        use_advanced_uri = false,
      })
    end,
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
    "thinca/vim-qfreplace",
    cmd = "Qfreplace",
  },
  {
    "haya14busa/vim-asterisk",
    event = { "VeryLazy" },
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
    event = { "VeryLazy" },
    config = function()
      require("toggleterm").setup({
        size = function(term)
          if term.direction == "horizontal" then
            return 20
          elseif term.direction == "vertical" then
            return vim.o.columns * 0.4
          end
        end,
        open_mapping = [[<c-t>]],
        shade_filetypes = {},
        shade_terminals = false,
        direction = "horizontal",
        insert_mappings = true,
        close_on_exit = false,
        persist_size = false,
        persist_mode = false,
      })
    end,
  },
  {
    "chomosuke/term-edit.nvim",
    ft = "toggleterm",
    version = "1.*",
    config = function()
      require("term-edit").setup({
        -- Mandatory option:
        -- Set this to a lua pattern that would match the end of your prompt.
        -- Or a table of multiple lua patterns where at least one would match the
        -- end of your prompt at any given time.
        -- For most bash/zsh user this is '%$ '.
        -- For most powershell/fish user this is '> '.
        -- For most windows cmd user this is '>'.
        prompt_end = "%$ ",
        -- How to write lua patterns: https://www.lua.org/pil/20.2.html
      })
    end,
  },
  {
    "airblade/vim-rooter",
    config = function()
      vim.g.rooter_patterns = { ".git", "composer.json", "package.json" }
    end,
  },
  { "machakann/vim-sandwich" },
  { "simeji/winresizer" },
  {
    "iamcco/markdown-preview.nvim",
    ft = { "markdown" },
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
    config = function()
      vim.g.mkdp_auto_start = 0
      vim.g.mkdp_auto_close = 0
      vim.g.mkdp_refresh_slow = 0
      vim.g.mkdp_command_for_global = 1
      vim.g.mkdp_open_to_the_world = 0
      vim.g.mkdp_open_ip = ""
      vim.g.mkdp_browser = ""
      vim.g.mkdp_echo_preview_url = 0
      vim.g.mkdp_browserfunc = ""
      vim.g.mkdp_preview_options = {
        mkit = {},
        katex = {},
        uml = {},
        maid = {},
        disable_sync_scroll = 0,
        sync_scroll_type = "middle",
        hide_yaml_meta = 1,
      }
      vim.g.mkdp_highlight_css = ""
      vim.g.mkdp_port = ""
      vim.g.mkdp_page_title = "「${name}」"
    end,
  },
  {
    "ellisonleao/glow.nvim",
    config = function()
      require("glow").setup({
        border = "rounded", -- floating window border config
        style = "dark", -- filled automatically with your current editor background, you can override using glow json style
        pager = false,
      })
    end,
    cmd = "Glow",
  },
  "osyo-manga/vim-over",
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {
      labels = "abcdefghijklmnopqrstuvwxyz",
      search = {
        -- search/jump in all windows
        multi_window = true,
        -- search direction
        forward = true,
        -- when `false`, find only matches in the given direction
        wrap = true,
        -- Each mode will take ignorecase and smartcase into account.
        -- * exact: exact match
        -- * search: regular search
        -- * fuzzy: fuzzy search
        -- * fun(str): custom function that returns a pattern
        --   For example, to only match at the beginning of a word:
        --   mode = function(str)
        --     return "\\<" .. str
        --   end,
        mode = "exact",
        -- behave like `incsearch`
        incremental = false,
        -- Excluded filetypes and custom window filters
        exclude = {
          "notify",
          "cmp_menu",
          "noice",
          "flash_prompt",
          function(win)
            -- exclude non-focusable windows
            return not vim.api.nvim_win_get_config(win).focusable
          end,
        },
        -- Optional trigger character that needs to be typed before
        -- a jump label can be used. It's NOT recommended to set this,
        -- unless you know what you're doing
        trigger = "",
        -- max pattern length. If the pattern length is equal to this
        -- labels will no longer be skipped. When it exceeds this length
        -- it will either end in a jump or terminate the search
        max_length = nil, ---@type number?
      },
      jump = {
        -- save location in the jumplist
        jumplist = true,
        -- jump position
        pos = "start", ---@type "start" | "end" | "range"
        -- add pattern to search history
        history = false,
        -- add pattern to search register
        register = false,
        -- clear highlight after jump
        nohlsearch = false,
        -- automatically jump when there is only one match
        autojump = false,
        -- You can force inclusive/exclusive jumps by setting the
        -- `inclusive` option. By default it will be automatically
        -- set based on the mode.
        inclusive = nil, ---@type boolean?
        -- jump position offset. Not used for range jumps.
        -- 0: default
        -- 1: when pos == "end" and pos < current position
        offset = nil, ---@type number
      },
      label = {
        -- allow uppercase labels
        uppercase = true,
        -- add a label for the first match in the current window.
        -- you can always jump to the first match with `<CR>`
        current = true,
        -- show the label after the match
        after = true, ---@type boolean|number[]
        -- show the label before the match
        before = false, ---@type boolean|number[]
        -- position of the label extmark
        style = "overlay", ---@type "eol" | "overlay" | "right_align" | "inline"
        -- flash tries to re-use labels that were already assigned to a position,
        -- when typing more characters. By default only lower-case labels are re-used.
        reuse = "lowercase", ---@type "lowercase" | "all"
        -- for the current window, label targets closer to the cursor first
        distance = true,
        -- minimum pattern length to show labels
        -- Ignored for custom labelers.
        min_pattern_length = 0,
      },
      -- show a backdrop with hl FlashBackdrop
      backdrop = true,
      -- Highlight the search matches
      matches = true,
      -- extmark priority
      priority = 5000,
      groups = {
        match = "FlashMatch",
        current = "FlashCurrent",
        backdrop = "FlashBackdrop",
        label = "FlashLabel",
      },
      -- action to perform when picking a label.
      -- defaults to the jumping logic depending on the mode.
      action = nil,
      -- initial pattern to use when opening flash
      pattern = "",
      -- When `true`, flash will try to continue the last search
      continue = false,
      -- You can override the default options for a specific mode.
      -- Use it with `require("flash").jump({mode = "forward"})`
      modes = {
        -- options used when flash is activated through
        -- `f`, `F`, `t`, `T`, `;` and `,` motions
        search = {
          enabled = false,
          labels = "abcdefghijklmnopqrstuvwxyz",
        },
        char = {
          enabled = true,
          -- by default all keymaps are enabled, but you can disable some of them,
          -- by removing them from the list.
          keys = { "f", "F", "t", "T", ";", "," },
          search = { wrap = false },
          highlight = { backdrop = true },
          jump = { register = false },
        },
        -- options used for treesitter selections
        -- `require("flash").treesitter()`
        treesitter = {
          labels = "abcdefghijklmnopqrstuvwxyz",
          jump = { pos = "range" },
        },
      },
    },
    keys = {
      {
        "ss",
        mode = { "n", "x", "o" },
        function()
          -- default options: exact mode, multi window, all directions, with a backdrop
          require("flash").jump()
        end,
        desc = "Flash",
      },
      {
        "Ss",
        mode = { "n", "o", "x" },
        function()
          -- show labeled treesitter nodes around the cursor
          require("flash").treesitter()
        end,
        desc = "Flash Treesitter",
      },
      {
        "r",
        mode = "o",
        function()
          -- jump to a remote location to execute the operator
          require("flash").remote()
        end,
        desc = "Remote Flash",
      },
      {
        "R",
        mode = { "n", "o", "x" },
        function()
          -- show labeled treesitter nodes around the search matches
          require("flash").treesitter_search()
        end,
        desc = "Treesitter Search",
      },
    },
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
    enabled = false,
    cmd = { "Telescope" },
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
            "--multiline",
          },
          prompt_prefix = "  ",
          scroll_strategy = "cycle",
          color_devicon = true,
          preview = {
            treesitter = false,
          },
          path_display = {
            filename_first = { -- ファイル名を先頭に表示し、パスはグレーで目立たない表示になる
              reverse_directories = true, -- パスが深いところから順の表示になる
            },
          },
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
          },
          find_files = {
            theme = "ivy",
          },
          grep_string = {
            theme = "ivy",
          },
          live_grep = {
            theme = "ivy",
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
  {
    "nvim-orgmode/telescope-orgmode.nvim",
    -- event = "VeryLazy",
    dependencies = {
      "nvim-orgmode/orgmode",
      "nvim-telescope/telescope.nvim",
    },
    keys = {
      {
        "<leader>osr",
        function()
          require("telescope").extensions.orgmode.refile_heading({ max_depth = 5 })
        end,
        desc = "Search refile heading",
      },
      {
        "<leader>osf",
        function()
          require("telescope").extensions.orgmode.search_headings({ max_depth = 5 })
        end,
        desc = "Search heading",
      },
      {
        "<leader>osl",
        function()
          require("telescope").extensions.orgmode.insert_link()
        end,
        desc = "Search insert link",
      },
    },
    config = function()
      require("telescope").load_extension("orgmode")
    end,
  },

  -- Git
  {
    "lewis6991/gitsigns.nvim",
    event = { "VeryLazy" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("gitsigns").setup({
        numhl = true,
        linehl = false,
        current_line_blame = true,
        current_line_blame_opts = {
          virt_text = true,
          virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
          delay = 1000,
          ignore_whitespace = false,
          virt_text_priority = 100,
        },
        current_line_blame_formatter = "    <author> • <summary>  󱋡 <author_time:%R> at <author_time:%Y/%m/%d %H:%M>",
        signs = {
          add = {
            text = "┃",
          },
          change = {
            text = "┃",
          },
          delete = {
            text = "┃",
          },
          topdelete = {
            text = "┃",
          },
          changedelete = {
            text = "┃",
          },
          untracked = {
            text = "│",
          },
        },
      })
    end,
  },
  {
    "pwntester/octo.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    cmd = { "Octo" },
    config = function()
      require("octo").setup({
        gh_env = {
          AQUA_ROOT_DIR = vim.env["AQUA_ROOT_DIR"],
          AQUA_GLOBAL_CONFIG = vim.env["AQUA_GLOBAL_CONFIG"],
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
        "<cmd>DiffviewOpen<cr>",
        mode = "n",
        desc = "Diffview Open",
      },
      {
        "<leader>gdr",
        "<cmd>DiffviewFileHistory %<cr>",
        mode = "n",
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
    "akinsho/git-conflict.nvim",
    version = "*",
    enabled = true,
    lazy = false,
    config = function()
      require("git-conflict").setup({
        default_mappings = false, -- disable buffer local mapping created by this plugin
        default_commands = true, -- disable commands created by this plugin
        disable_diagnostics = false, -- This will disable the diagnostics in a buffer whilst it is conflicted
        list_opener = "copen", -- command or function to open the conflicts list
        highlights = { -- They must have background color, otherwise the default color will be used
          incoming = "DiffAdd",
          current = "DiffText",
        },
      })
    end,
  },
  {
    "NeogitOrg/neogit",
    lazy = true,
    enabled = function()
      return vim.g.git_client_type == "neogit"
    end,
    dependencies = { "nvim-lua/plenary.nvim", "sindrets/diffview.nvim" },
    config = function()
      local neogit = require("neogit")
      neogit.setup({
        kind = "auto",
        commit_editor = {
          kind = "split",
        },
        commit_select_view = {
          kind = "split",
        },
        commit_view = {
          kind = "split",
          verify_commit = os.execute("which gpg") == 0, -- Can be set to true or false, otherwise we try to find the binary
        },
        log_view = {
          kind = "tab",
        },
        rebase_editor = {
          kind = "auto",
        },
        reflog_view = {
          kind = "tab",
        },
        merge_editor = {
          kind = "auto",
        },
        tag_editor = {
          kind = "auto",
        },
        preview_buffer = {
          kind = "split",
        },
        popup = {
          kind = "split",
        },
        disable_signs = false,
        disable_line_numbers = true,
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
      })
    end,
  },

  -- Snippet
  "mattn/vim-sonictemplate",

  -- Linter
  {
    "nvimtools/none-ls.nvim",
    event = { "VeryLazy" },
    enabled = function()
      return false
    end,
    dependencies = {
      "nvimtools/none-ls-extras.nvim",
      "neovim/nvim-lspconfig",
      "nvim-lua/plenary.nvim",
      "davidmh/cspell.nvim",
    },
    config = function()
      local null_ls = require("null-ls")
      local cspell = require("cspell")

      local cspell_config = {
        find_json = function(_)
          local path = vim.fs.joinpath("~/ghq/github.com/tkmpypy/dotfiles/.config/nvim", "cspell", "cspell.json")
          return vim.fn.expand(path)
        end,
      }

      null_ls.setup({
        temp_dir = vim.fn.stdpath("cache"),
        sources = {
          cspell.code_actions.with({ config = cspell_config }),
          require("none-ls.code_actions.eslint_d"),
          cspell.diagnostics.with({
            config = cspell_config,
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
          null_ls.builtins.diagnostics.phpstan.with({
            -- to_temp_file = false,
            -- timeout = 100000,
            method = null_ls.methods.DIAGNOSTICS,
            args = { "analyze", "--memory-limit=-1", "--error-format", "json", "--no-progress" },
          }),
          null_ls.builtins.diagnostics.golangci_lint.with({
            timeout = 50000,
          }),
          null_ls.builtins.diagnostics.eslint,
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
    "mfussenegger/nvim-lint",
    enabled = function()
      return true
    end,
    config = function()
      local lint = require("lint")

      lint.linters_by_ft = {
        -- markdown = { "vale" },
        go = { "golangcilint" },
        python = { "ruff" },
        sh = { "shellcheck" },
        javascript = { "biomejs" },
        javascriptreact = { "eslint_d" },
        typescript = { "biomejs" },
        typescriptreact = { "eslint_d" },
        vue = { "eslint_d" },
        json = { "jsonlint" },
        php = { "phpstan" },
      }

      local phpstan = lint.linters.phpstan
      table.insert(phpstan.args, "--memory-limit=-1")

      local cspell = lint.linters.cspell
      cspell.args = {
        "lint",
        "--no-color",
        "--no-progress",
        "--no-summary",
        "--config",
        vim.fs.joinpath(vim.fn.stdpath("config"), "cspell", "cspell.json"),
        function()
          return "stdin://" .. vim.api.nvim_buf_get_name(0)
        end,
      }

      local ignore_ft = {
        "neo-tree",
        "neo-tree-popup",
        "notify",
        "toggleterm",
        "alpha",
      }
      local ignore_bt = {
        "terminal",
        "quickfix",
      }
      vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost" }, {
        group = vim.api.nvim_create_augroup("lint", { clear = true }),
        callback = function()
          local buf = vim.api.nvim_get_current_buf()
          local ft = vim.bo[buf].filetype
          local bt = vim.bo[buf].buftype
          if vim.list_contains(ignore_ft, ft) then
            vim.notify("this filetype is ignored: " .. ft, { title = "[lint]" })
            return
          elseif vim.list_contains(ignore_bt, bt) then
            vim.notify("this buftype is ignored: " .. bt, { title = "[lint]" })
            return
          end

          require("lint").try_lint()
          require("lint").try_lint("cspell")
        end,
      })
    end,
  },

  -- Formatter
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo", "Format", "FormatEnable", "FormatDisable" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    keys = {
      {
        -- Customize or remove this keymap to your liking
        "<leader>F",
        function()
          require("conform").format({ async = true, lsp_format = "fallback" })
        end,
        mode = "",
        desc = "Format buffer",
      },
    },
    config = function()
      local conform = require("conform")
      conform.setup({
        notify_on_error = true,
        -- If this is set, Conform will run the formatter asynchronously after save.
        -- It will pass the table to conform.format().
        -- This can also be a function that returns the table.
        format_after_save = function(bufnr)
          -- Disable with a global or buffer-local variable
          if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
            return
          end
          return { timeout_ms = 2000, lsp_format = "fallback", async = true }
        end,
        formatters_by_ft = {
          lua = { "stylua" },
          go = { "goimports", "gofumpt", "gci" },
          python = { "isort", "autopep8" },
          terraform = { "terraform_fmt" },
          sh = { "shfmt" },
          rust = { "rustfmt" },
          php = { "pint" },
          -- blade = { "blade-formatter" },
          sql = { "sql_formatter" },
          javascript = { "biome" },
          javascriptreact = { "biome" },
          typescript = { "biome" },
          typescriptreact = { "biome" },
          css = { "prettierd", "prettier", stop_after_first = true },
          scss = { "prettierd", "prettier", stop_after_first = true },
          less = { "prettierd", "prettier", stop_after_first = true },
          html = { "prettierd", "prettier", stop_after_first = true },
          json = { "prettierd", "prettier", stop_after_first = true },
          jsonc = { "prettierd", "prettier", stop_after_first = true },
          yaml = { "prettierd", "prettier", stop_after_first = true },
          markdown = { "prettierd", "prettier", stop_after_first = true },
        },
      })

      conform.formatters["blade-formatter"] = {
        prepend_args = { "-i", "2" },
      }

      vim.api.nvim_create_user_command("Format", function(args)
        local range = nil
        if args.count ~= -1 then
          local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
          range = {
            start = { args.line1, 0 },
            ["end"] = { args.line2, end_line:len() },
          }
        end
        require("conform").format({ async = true, lsp_format = "fallback", range = range })
      end, { range = true })

      -- disable format on save(default)
      vim.b.disable_autoformat = false
      vim.g.disable_autoformat = false
      vim.api.nvim_create_user_command("FormatDisable", function(args)
        if args.bang then
          -- FormatDisable! will disable formatting just for this buffer
          vim.b.disable_autoformat = true
        else
          vim.g.disable_autoformat = true
        end
      end, {
        desc = "Disable autoformat-on-save",
        bang = true,
      })
      vim.api.nvim_create_user_command("FormatEnable", function()
        vim.b.disable_autoformat = false
        vim.g.disable_autoformat = false
      end, {
        desc = "Re-enable autoformat-on-save",
      })
    end,
  },

  -- LSP
  {
    "rachartier/tiny-inline-diagnostic.nvim",
    event = "VeryLazy",
    priority = 1000, -- needs to be loaded in first
    config = function()
      require("tiny-inline-diagnostic").setup({
        preset = "modern",
        options = {
          show_source = true,
          show_all_diags_on_cursorline = true,
          multilines = {
            -- Enable multiline diagnostic messages
            enabled = true,

            -- Always show messages on all lines for multiline diagnostics
            always_show = true,
          },
        },
      })
    end,
  },
  {
    "j-hui/fidget.nvim",
    event = { "LspAttach" },
    opts = {
      notification = {
        window = {
          winblend = 0,
        },
      },
    },
  },
  {
    "mason-org/mason.nvim",
    cmd = { "Mason", "MasonLog", "MasonInstall", "MasonUninstall", "MasonUninstallAll" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    version = "^1.0.0",
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
    "mason-org/mason-lspconfig.nvim",
    -- dependencies = {"mason-org/mason.nvim"},
    event = { "VeryLazy" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    -- See: https://github.com/mason-org/mason.nvim/issues/1929
    version = "^1.0.0", -- NOTE: `attempt to call field 'setup_handlers' (a nil value)`
    config = function()
      require("mason-lspconfig").setup()
    end,
  },
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    event = { "VeryLazy" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    config = function()
      require("mason-tool-installer").setup({

        -- a list of all tools you want to ensure are installed upon
        -- start; they should be the names Mason uses for each tool
        ensure_installed = {

          -- you can pin a tool to a particular version
          -- { "golangci-lint", version = "v1.47.0" },

          -- you can turn off/on auto_update per tool
          -- { "bash-language-server", auto_update = true },

          -- LSP
          "lua-language-server",
          "vim-language-server",
          "gopls",
          -- "phpactor",
          "intelephense",
          "rust-analyzer",
          "css-lsp",
          "dockerfile-language-server",
          "vtsls",
          "html-lsp",
          "json-lsp",
          "yaml-language-server",
          "buf-language-server",
          "pyright",
          "marksman",
          "svelte-language-server",
          "tailwindcss-language-server",
          "terraform-ls",
          -- Linter
          "buf",
          "cspell",
          "golangci-lint",
          "shellcheck",
          "eslint_d",
          "phpstan",
          -- Formatte
          "prettierd",
          "sql-formatter",
          "black",
          "gofumpt",
          "goimports",
          "gci",
          "shfmt",
          "stylua",
          "blade-formatter",
          -- "pint",
          "php-cs-fixer",
        },

        -- if set to true this will check each tool for updates. If updates
        -- are available the tool will be updated. This setting does not
        -- affect :MasonToolsUpdate or :MasonToolsInstall.
        -- Default: false
        auto_update = false,

        -- automatically install / update on startup. If set to false nothing
        -- will happen on startup. You can use :MasonToolsInstall or
        -- :MasonToolsUpdate to install tools and check for updates.
        -- Default: true
        run_on_start = false,

        -- set a delay (in ms) before the installation starts. This is only
        -- effective if run_on_start is set to true.
        -- e.g.: 5000 = 5 second delay, 10000 = 10 second delay, etc...
        -- Default: 0
        start_delay = 3000, -- 3 second delay

        -- Only attempt to install if 'debounce_hours' number of hours has
        -- elapsed since the last time Neovim was started. This stores a
        -- timestamp in a file named stdpath('data')/mason-tool-installer-debounce.
        -- This is only relevant when you are using 'run_on_start'. It has no
        -- effect when running manually via ':MasonToolsInstall' etc....
        -- Default: nil
        debounce_hours = 5, -- at least 5 hours between attempts to install/update
      })
    end,
  },
  {
    "neovim/nvim-lspconfig",
    event = { "VeryLazy" },
    dependencies = { "mason-org/mason-lspconfig.nvim" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    config = function()
      require("lsp")
    end,
  },
  {
    "Bekaboo/dropbar.nvim",
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    opts = {
      icons = {
        kinds = {
          symbols = {
            Array = "󰅪 ",
            Boolean = " ",
            BreakStatement = "󰙧 ",
            Call = "󰃷 ",
            CaseStatement = "󱃙 ",
            Class = " ",
            Color = "󰏘 ",
            Constant = "󰏿 ",
            Constructor = " ",
            ContinueStatement = "→ ",
            Copilot = " ",
            Declaration = "󰙠 ",
            Delete = "󰩺 ",
            DoStatement = "󰑖 ",
            Enum = " ",
            EnumMember = " ",
            Event = " ",
            Field = " ",
            File = "󰈔 ",
            Folder = "󰉋 ",
            ForStatement = "󰑖 ",
            Function = "󰊕 ",
            Identifier = "󰀫 ",
            IfStatement = "󰇉 ",
            Interface = " ",
            Keyword = "󰌋 ",
            List = "󰅪 ",
            Log = "󰦪 ",
            Lsp = " ",
            Macro = "󰁌 ",
            MarkdownH1 = "󰉫 ",
            MarkdownH2 = "󰉬 ",
            MarkdownH3 = "󰉭 ",
            MarkdownH4 = "󰉮 ",
            MarkdownH5 = "󰉯 ",
            MarkdownH6 = "󰉰 ",
            Method = "󰆧 ",
            Module = "󰏗 ",
            Namespace = "󰅩 ",
            Null = "󰢤 ",
            Number = "󰎠 ",
            Object = "󰅩 ",
            Operator = "󰆕 ",
            Package = "󰆦 ",
            Property = " ",
            Reference = "󰦾 ",
            Regex = " ",
            Repeat = "󰑖 ",
            Scope = "󰅩 ",
            Snippet = "󰩫 ",
            Specifier = "󰦪 ",
            Statement = "󰅩 ",
            String = "󰉾 ",
            Struct = " ",
            SwitchStatement = "󰺟 ",
            Text = " ",
            Type = " ",
            TypeParameter = "󰆩 ",
            Unit = " ",
            Value = "󰎠 ",
            Variable = "󰀫 ",
            WhileStatement = "󰑖 ",
          },
        },
        ui = {
          bar = {
            separator = "  ",
            extends = "…",
          },
          menu = {
            separator = " ",
            indicator = "  ",
          },
        },
      },
      bar = {
        padding = {
          left = 1,
          right = 1,
        },
        pick = {
          pivots = "abcdefghijklmnopqrstuvwxyz",
        },
        truncate = true,
        sources = function(buf, _)
          local sources = require("dropbar.sources")
          local utils = require("dropbar.utils")
          if vim.bo[buf].buftype == "terminal" then
            return {
              sources.terminal,
            }
          end

          local s = { sources.lsp }
          if require("nvim-treesitter.parsers").has_parser() then
            s[#s + 1] = sources.treesitter
          end
          if vim.bo[buf].ft == "markdown" then
            s[#s + 1] = sources.markdown
          end

          return {
            sources.path,
            utils.source.fallback(s),
          }
        end,
      },
      menu = {
        entry = {
          padding = {
            left = 1,
            right = 1,
          },
        },
        ---@type table<string, string|function|table<string, string|function>>
        keymaps = {
          ["<LeftMouse>"] = function()
            local api = require("dropbar.api")
            local menu = api.get_current_dropbar_menu()
            if not menu then
              return
            end
            local mouse = vim.fn.getmousepos()
            if mouse.winid ~= menu.win then
              local parent_menu = api.get_dropbar_menu(mouse.winid)
              if parent_menu and parent_menu.sub_menu then
                parent_menu.sub_menu:close()
              end
              if vim.api.nvim_win_is_valid(mouse.winid) then
                vim.api.nvim_set_current_win(mouse.winid)
              end
              return
            end
            menu:click_at({ mouse.line, mouse.column }, nil, 1, "l")
          end,
          ["<CR>"] = function()
            local menu = require("dropbar.api").get_current_dropbar_menu()
            if not menu then
              return
            end
            local cursor = vim.api.nvim_win_get_cursor(menu.win)
            local component = menu.entries[cursor[1]]:first_clickable(cursor[2])
            if component then
              menu:click_on(component, nil, 1, "l")
            end
          end,
          ["<ESC>"] = function()
            local menu = require("dropbar.api").get_current_dropbar_menu()
            menu:close()
          end,
        },
      },
    },
  },
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {
      library = {
        -- Library items can be absolute paths
        -- "~/projects/my-awesome-lib",
        -- Or relative, which means they will be resolved as a plugin
        -- "LazyVim",
        -- When relative, you can also provide a path to the library in the plugin dir
        "luvit-meta/library", -- see below
      },
    },
  },
  {
    "Bilal2453/luvit-meta",
    lazy = true,
  },
  {
    "someone-stole-my-name/yaml-companion.nvim",
    ft = "yaml",
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    dependencies = {
      "nvim-lua/plenary.nvim",
      -- "nvim-telescope/telescope.nvim",
      "neovim/nvim-lspconfig",
    },
    config = function()
      -- require("telescope").load_extension("yaml_schema")
    end,
  },
  {
    "simrat39/rust-tools.nvim",
    ft = "rust",
    dependencies = { "neovim/nvim-lspconfig" },
  },
  {
    "b0o/schemastore.nvim",
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    event = { "VeryLazy" },
    dependencies = { "neovim/nvim-lspconfig" },
  },
  {
    "zeioth/garbage-day.nvim",
    enabled = false,
    dependencies = { "neovim/nvim-lspconfig" },
    event = { "LspAttach" },
    opts = {
      grace_period = 60 * 15,
      excluded_lsp_clients = {
        "jdtls",
        "marksman",
        "vtsls",
        "hls",
        "rust_analyzer",
      },
      stop_invisible = false,
      notifications = true,
    },
  },
  {
    "coder/claudecode.nvim",
    dependencies = { "folke/snacks.nvim" },
    keys = {
      { "<leader>z", nil, desc = "AI/Claude Code" },
      { "<leader>zc", "<cmd>ClaudeCode<cr>", desc = "Toggle Claude" },
      { "<leader>zt", "<cmd>ClaudeCodeFocus<cr>", desc = "Focus Claude" },
      { "<M-,>", "<cmd>ClaudeCodeFocus<cr>", desc = "Claude Code (Alt+,)", mode = { "n", "x" } },
      { "<leader>zr", "<cmd>ClaudeCode --resume<cr>", desc = "Resume Claude" },
      { "<leader>zC", "<cmd>ClaudeCode --continue<cr>", desc = "Continue Claude" },
      { "<leader>zb", "<cmd>ClaudeCodeAdd %<cr>", desc = "Add current buffer" },
      { "<leader>zs", "<cmd>ClaudeCodeSend<cr>", mode = "v", desc = "Send to Claude" },
      {
        "<leader>zs",
        "<cmd>ClaudeCodeTreeAdd<cr>",
        desc = "Add file",
        ft = { "NvimTree", "neo-tree", "oil" },
      },
      -- Diff management
      { "<leader>za", "<cmd>ClaudeCodeDiffAccept<cr>", desc = "Accept diff" },
      { "<leader>zd", "<cmd>ClaudeCodeDiffDeny<cr>", desc = "Deny diff" },
    },
    opts = {
      terminal = {
        ---@module "snacks"
        ---@type snacks.win.Config|{}
        snacks_win_opts = {
          position = "float",
          width = 0.85,
          height = 0.85,
          border = "rounded",
          keys = {
            claude_hide_alt = {
              "<M-,>",
              function(self)
                self:hide()
              end,
              mode = "t",
              desc = "Hide (Alt+,)",
            },
          },
        },
      },
      -- Diff Integration
      diff_opts = {
        auto_close_on_accept = true,
        vertical_split = true,
        open_in_current_tab = true,
        keep_terminal_focus = true, -- If true, moves focus back to terminal after diff opens
      },
    },
  },
  {
    "olimorris/codecompanion.nvim",
    -- lazy = false, -- lazy loading handled internally
    dependencies = {
      "j-hui/fidget.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    keys = {
      { "<leader>Z", nil, desc = "+Code Companion" },
      {
        "<leader>Zz",
        "<cmd>CodeCompanion<cr>",
        desc = "CodeCompanion",
        mode = { "n", "v" },
      },
      {
        "<leader>Za",
        "<cmd>CodeCompanionActions<cr>",
        desc = "CodeCompanion Actions",
        mode = { "n", "v" },
      },
      {
        "<leader>Zt",
        "<cmd>CodeCompanionChat Toggle<cr>",
        desc = "Toggle CodeCompanion Chat",
        mode = { "n", "v" },
      },
    },
    init = function()
      require("codecompanion.fidget-spinner"):init()
    end,
    config = function()
      require("codecompanion").setup({
        opts = {
          language = "Japanese",
        },
        adapters = {
          copilot = function()
            return require("codecompanion.adapters").extend("copilot", {
              schema = {
                model = {
                  default = "claude-3.7-sonnet",
                },
              },
            })
          end,
        },
        strategies = {
          chat = {
            adapter = "copilot",
            keymaps = {
              send = {
                modes = { n = { "<CR>", "<C-s>" }, i = "<C-s>" },
              },
              close = {
                modes = { n = "q", i = "<M-q>" },
              },
              -- Add further custom keymaps here
            },
            slash_commands = {
              ["git_files"] = {
                description = "List git files",
                ---@param chat CodeCompanion.Chat
                callback = function(chat)
                  local handle = io.popen("git ls-files")
                  if handle ~= nil then
                    local result = handle:read("*a")
                    handle:close()
                    chat:add_reference({ role = "user", content = result }, "git", "<git_files>")
                  else
                    return vim.notify("No git files available", vim.log.levels.INFO, { title = "CodeCompanion" })
                  end
                end,
                opts = {
                  contains_code = false,
                },
              },
            },
          },
          inline = {
            adapter = "copilot",
          },
          cmd = {
            adapter = "copilot",
          },
        },
      })
    end,
  },
  {
    "zbirenbaum/copilot.lua",
    cmd = { "Copilot" },
    event = { "InsertEnter" },
    config = function()
      require("copilot").setup({
        suggestion = { enabled = false },
        panel = { enabled = false },
      })
    end,
  },
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    enabled = false,
    build = "make tiktoken",
    dependencies = {
      { "zbirenbaum/copilot.lua" }, -- or github/copilot.vim
      { "nvim-lua/plenary.nvim" }, -- for curl, log wrapper
    },
    cmd = {
      "Copilot",
      "CopilotChat",
      "CopilotChatReview",
      "CopilotChatRefactor",
      "CopilotChatCommit",
      "CopilotChatCommitStaged",
      "CopilotChatExplain",
      "CopilotChatTests",
      "CopilotChatFix",
      "CopilotChatOptimize",
      "CopilotChatDocs",
      "CopilotChatDocsJA",
      "CopilotChatFixDiagnostic",
    },
    config = function()
      local select = require("CopilotChat.select")
      require("CopilotChat").setup({
        -- debug = true,
        -- See Configuration section for rest
        context = "files",
        model = "claude-3.7-sonnet",
        window = {
          layout = "vertical", -- 'vertical', 'horizontal', 'float', 'replace'
          width = 0.3, -- fractional width of parent, or absolute width in columns when > 1
          height = 1.0, -- fractional height of parent, or absolute height in rows when > 1
          -- Options below only apply to floating windows
          relative = "editor", -- 'editor', 'win', 'cursor', 'mouse'
          border = "single", -- 'none', single', 'double', 'rounded', 'solid', 'shadow'
          row = nil, -- row position of the window, default is centered
          col = nil, -- column position of the window, default is centered
          title = "Copilot Chat", -- title of chat window
          footer = nil, -- footer of chat window
          zindex = 1, -- determines if window is on top or below other floating windows
        },
        prompts = {
          Explain = {
            prompt = "カーソル上のコードの説明を段落をつけて書いてください。",
            system_prompt = "COPILOT_EXPLAIN",
          },
          Tests = {
            prompt = "カーソル上のコードの詳細な単体テスト関数を書いてください。",
            system_prompt = "COPILOT_TESTS",
          },
          Fix = {
            prompt = "このコードには問題があります。バグを修正したコードに書き換えてください。",
            system_prompt = "COPILOT_FIX",
          },
          Review = {
            prompt = "選択したコードを日本語でレビューしてください",
            system_prompt = "COPILOT_REVIEW",
          },
          Refactor = {
            prompt = "選択したコードを最適化し、パフォーマンスと可読性を向上させてください。",
            system_prompt = "COPILOT_REFACTOR",
          },
          Docs = {
            prompt = "選択したコードのドキュメントを書いてください。ドキュメントをコメントとして追加した元のコードを含むコードブロックで回答してください。使用するプログラミング言語に最も適したドキュメントスタイルを使用してください（例：JavaScriptのJSDoc、Pythonのdocstringsなど）",
            system_prompt = "COPILOT_DOCS",
          },
          DocsJA = {
            prompt = "選択したコードのドキュメントを日本語で書いてください（句読点は不要）。ドキュメントをコメントとして追加した元のコードを含むコードブロックで回答してください。使用するプログラミング言語に最も適したドキュメントスタイルを使用してください（例：JavaScriptのJSDoc、Pythonのdocstringsなど）",
            system_prompt = "COPILOT_DOCS_JA",
          },
          FixDiagnostic = {
            prompt = "ファイル内の次のような診断上の問題を解決してください：",
            selection = select.diagnostics,
          },
        },
      })
    end,
    -- See Commands section for default commands if you want to lazy load on them
  },
  {
    "saghen/blink.cmp",
    enabled = function()
      return vim.g.complete_engine_type == "blink"
    end,
    lazy = false, -- lazy loading handled internally
    -- optional: provides snippets for the snippet source
    dependencies = {
      "rafamadriz/friendly-snippets",
      "giuxtaposition/blink-cmp-copilot",
      "moyiz/blink-emoji.nvim",
      "echasnovski/mini.icons",
      {
        "saghen/blink.pairs",
        version = "*", -- (recommended) only required with prebuilt binaries
        -- download prebuilt binaries from github releases
        dependencies = "saghen/blink.download",
        config = true,
      },
      -- {
      --   "windwp/nvim-autopairs",
      --   opts = {
      --     map_cr = true,
      --   },
      -- },
    },

    -- use a release tag to download pre-built binaries
    version = "v1.*",
    -- OR build from source, requires nightly: https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust
    -- build = "cargo build --release",
    -- On musl libc based systems you need to add this flag
    -- build = 'RUSTFLAGS="-C target-feature=-crt-static" cargo build --release',

    opts = {
      fuzzy = {
        implementation = "prefer_rust",
      },
      keymap = {
        preset = "default",
        ["<CR>"] = { "accept", "fallback" },
        ["<C-e>"] = { "hide", "fallback" },
        ["<C-p>"] = { "select_prev", "fallback" },
        ["<C-n>"] = { "select_next", "fallback" },
        ["<C-j>"] = { "snippet_forward", "fallback" },
        ["<C-k>"] = { "snippet_backward", "fallback" },
      },
      completion = {
        keyword = {
          range = "full",
        },
        trigger = {
          prefetch_on_insert = true,
        },
        -- Don't select by default, auto insert on selection
        list = { selection = { preselect = false, auto_insert = true } },
        -- or set either per mode via a function
        -- list = { selection = { preselect = function(ctx) return ctx.mode ~= 'cmdline' end
        accept = {
          auto_brackets = {
            enabled = true,
          },
        },
        menu = {
          border = "single",
          draw = {
            align_to = "kind_icon",
            columns = { { "kind_icon", "label", "label_description", "kind", gap = 1 }, { "source_name" } },
            treesitter = { "lsp" },
            components = {
              kind_icon = {
                ellipsis = false,
                text = function(ctx)
                  local kind_icon, _, _ = require("mini.icons").get("lsp", ctx.kind)
                  return kind_icon
                end,
                -- Optionally, you may also use the highlights from mini.icons
                highlight = function(ctx)
                  local _, hl, _ = require("mini.icons").get("lsp", ctx.kind)
                  return hl
                end,
              },
            },
          },
        },
        documentation = {
          auto_show = true,
          window = {
            border = "single",
          },
        },
        -- Displays a preview of the selected item on the current line
        ghost_text = {
          enabled = true,
        },
      },
      cmdline = {
        enabled = true,
        keymap = { preset = "cmdline" },
        completion = {
          list = {
            selection = {
              -- When `true`, will automatically select the first item in the completion list
              preselect = false,
              -- When `true`, inserts the completion item automatically when selecting it
              auto_insert = true,
            },
          },
          -- Whether to automatically show the window when new completion items are available
          menu = { auto_show = true },
          -- Displays a preview of the selected item on the current line
          ghost_text = { enabled = true },
        },
      },
      signature = {
        enabled = true,
      },
      sources = {
        default = { "lsp", "path", "snippets", "buffer", "copilot", "emoji" },
        min_keyword_length = 0,
        -- Please see https://github.com/Saghen/blink.compat for using `nvim-cmp` sources
        providers = {
          lsp = {
            name = "LSP",
            module = "blink.cmp.sources.lsp",

            --- *All* of the providers have the following options available
            --- NOTE: All of these options may be functions to get dynamic behavior
            --- See the type definitions for more information
            enabled = true, -- whether or not to enable the provider
            async = true,
            transform_items = nil, -- function to transform the items before they're returned
            should_show_items = true, -- whether or not to show the items
            max_items = nil, -- maximum number of items to return
            min_keyword_length = 0, -- minimum number of characters to trigger the provider
            fallbacks = {}, -- if any of these providers return 0 items, it will fallback to this provider
            score_offset = 0, -- boost/penalize the score of the items
            override = nil, -- override the source's functions
          },
          path = {
            name = "Path",
            module = "blink.cmp.sources.path",
            score_offset = 3,
            opts = {
              trailing_slash = false,
              label_trailing_slash = true,
              get_cwd = function(context)
                return vim.fn.expand(("#%d:p:h"):format(context.bufnr))
              end,
              show_hidden_files_by_default = true,
            },
          },
          snippets = {
            name = "Snippets",
            module = "blink.cmp.sources.snippets",
            score_offset = -3,
            opts = {
              friendly_snippets = true,
              search_paths = { vim.fn.stdpath("config") .. "/snippets" },
              global_snippets = { "all" },
              extended_filetypes = {},
              ignored_filetypes = {},
            },

            --- Example usage for disabling the snippet provider after pressing trigger characters (i.e. ".")
            -- enabled = function(ctx) return ctx ~= nil and ctx.trigger.kind == vim.lsp.protocol.CompletionTriggerKind.TriggerCharacter end,
          },
          buffer = {
            name = "Buffer",
            module = "blink.cmp.sources.buffer",
            opts = {
              -- default to all visible buffers
              get_bufnrs = function()
                return vim.tbl_filter(function(bufnr)
                  return vim.bo[bufnr].buftype == ""
                end, vim.api.nvim_list_bufs())
              end,
            },
          },
          copilot = {
            name = "copilot",
            module = "blink-cmp-copilot",
          },
          emoji = {
            module = "blink-emoji",
            name = "Emoji",
            score_offset = 15, -- Tune by preference
            opts = { insert = true }, -- Insert emoji (default) or complete its name
            should_show_items = function()
              return vim.tbl_contains(
                -- Enable emoji completion only for git commits and markdown.
                -- By default, enabled for all file-types.
                { "gitcommit", "markdown", "" },
                vim.o.filetype
              )
            end,
          },
        },
      },
    },
  },
  {
    "hrsh7th/nvim-cmp",
    event = { "InsertEnter", "CmdlineEnter" },
    enabled = function()
      return vim.g.complete_engine_type == "cmp"
    end,
    dependencies = {
      {
        "windwp/nvim-autopairs",
        opts = {
          map_cr = true,
        },
      },
      { "onsails/lspkind.nvim" },
      {
        "L3MON4D3/LuaSnip",
        version = "v2.*",
        build = "make install_jsregexp",
        dependencies = {
          "rafamadriz/friendly-snippets",
        },
        config = function()
          require("luasnip").setup({
            history = true,
            delete_check_events = "TextChanged",
          })
          require("luasnip").log.set_loglevel("debug")
          require("luasnip.loaders.from_vscode").lazy_load()
          require("luasnip.loaders.from_vscode").lazy_load({ paths = { "~/.config/nvim/snippets" } })
        end,
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
            filetypes = { "NeogitCommitMessage", "gitcommit", "octo" },
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
      -- {
      --   "zbirenbaum/copilot-cmp",
      --   config = function()
      --     require("copilot_cmp").setup({
      --       -- event = { "InsertEnter", "LspAttach" },
      --       -- fix_pairs = true,
      --     })
      --   end,
      -- },
    },
    config = function()
      local cmp = require("cmp")
      local types = require("cmp.types")
      local compare = require("cmp.config.compare")
      local lspkind = require("lspkind")

      cmp.setup({
        enabled = function()
          return vim.api.nvim_get_option_value("buftype", { buf = 0 }) ~= "prompt"
        end,
        performance = {
          debounce = 1,
          throttle = 10,
        },
        -- view = {
        --   entries = { name = "custom", selection_order = "top_down" },
        -- },
        window = {
          -- • "none": No border (default).
          -- • "single": A single line box.
          -- • "double": A double line box.
          -- • "rounded": Like "single", but with rounded corners ("╭"
          --   etc.).
          -- • "solid": Adds padding by a single whitespace cell.
          -- • "shadow": A drop shadow effect by blending with the
          --   background.
          completion = cmp.config.window.bordered({ border = "single" }),
          documentation = cmp.config.window.bordered({ border = "single" }),
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
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
        }),

        -- You should specify your *installed* sources.
        sources = cmp.config.sources({
          {
            name = "nvim_lsp",
          },
          {
            -- name = "vsnip",
            name = "luasnip",
          },
          {
            name = "nvim_lua",
          },
          {
            name = "buffer",
            max_item_count = 10,
            keyword_length = 1,
            option = {
              get_bufnrs = function()
                return vim.api.nvim_list_bufs()
              end,
            },
          },
          {
            name = "path",
            max_item_count = 10,
          },
          -- { name = "nvim_lsp_signature_help" },
        }),
        formatting = {
          format = lspkind.cmp_format({
            symbol_map = {
              Copilot = "",
            },
            mode = "symbol_text",
            menu = {
              buffer = "[BUF]",
              nvim_lsp = "[LSP]",
              path = "[PATH]",
              vsnip = "[SNIP]",
              luasnip = "[SNIP]",
              nvim_lua = "[LUA]",
              orgmode = "[ORG]",
              Copilot = "[COPILOT]",
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

      require("cmp.conventionalprefix")
      cmp.setup.filetype({ "NeogitCommitMessage", "gitcommit" }, {
        sources = cmp.config.sources({
          { name = "conventionalprefix" },
          { name = "git" },
          { name = "buffer" },
        }),
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
        matching = { disallow_symbol_nonprefix_matching = false },
      })

      -- Workaround: `vim.ui.input` で組み込みの保管機能を使うため
      -- https://github.com/hrsh7th/nvim-cmp/issues/1511#issuecomment-1743055767
      vim.keymap.set("c", "<tab>", "<C-z>", { silent = false }) -- to fix cmp
    end,
  },
  {
    "hrsh7th/nvim-gtd",
    event = { "VeryLazy" },
    dependencies = { "neovim/nvim-lspconfig" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    config = function()
      require("gtd").setup({})
    end,
  },
  {
    "folke/trouble.nvim",
    dependencies = { "neovim/nvim-lspconfig" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    cmd = { "Trouble", "TroubleToggle", "TroubleClose", "TroubleRefresh" },
    config = function()
      require("trouble").setup()
    end,
  },
  {
    "neoclide/coc.nvim",
    event = "VeryLazy",
    enabled = function()
      return vim.g.lsp_client_type == "coc"
    end,
    build = "npm ci",
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
        "@yaegassy/coc-intelephense",
        "@yaegassy/coc-laravel",
        "coc-blade",
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
        icons = {
          breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
          separator = "➜", -- symbol used between a key and it's label
          group = "+", -- symbol prepended to a group
        },
        layout = {
          height = { min = 8, max = 25 }, -- min and max height of the columns
          width = { min = 20, max = 50 }, -- min and max width of the columns
          spacing = 3, -- spacing between columns
          align = "left", -- align columns left, center or right
        },
        show_help = true, -- show help message on the command line when the popup is visible
        -- triggers = "auto", -- automatically setup triggers
        -- triggers = {"<leader>"} -- or specify a list manually
      })

      -- mode: v
      wk.add({
        mode = "v",
        { "<C-j>", ":m '>+1<CR>gv=gv", desc = "range down" },
        { "<C-k>", ":m '<-2<CR>gv=gv", desc = "range up" },
        { "<leader>x", group = "+Translate" },
        { "<leader>xe", ":Translate EN<CR>", desc = "to English" },
        { "<leader>xj", ":Translate JA<CR>", desc = "to Japanese" },
        { "<leader>xE", ":Translate EN -output=replace<CR>", desc = "replace to English" },
        { "<leader>xJ", ":Translate JA -output=replace<CR>", desc = "replace to Japanese" },
      })

      -- mode: n
      wk.add({
        mode = "n",
        { "R", "<Plug>(operator-replace)", desc = "Replace" },
        { "<leader>uc", group = "+Color" },
        { "<leader>ucp", "<cmd>CccPick<CR>", desc = "Pick color" },
        { "<leader>w", group = "+Window" },
        { "<leader>ww", "<cmd>Chowcho<cr>", desc = "Selector" },
        { "<leader>wr", "<cmd>WinResizerStartResize", desc = "Resize" },
        { "<leader>s", group = "+Search" },
        -- {
        --   "<leader>sb",
        --   '<cmd>lua require("telescope.builtin").buffers{ show_all_buffers = true, generic_sorters = require("telescope.sorters").fuzzy_with_index_bias }<CR>',
        --   desc = "Buffer",
        -- },
        -- { "<leader>sm", '<cmd>lua require("telescope.builtin").keymaps{ }<CR>', desc = "Keymaps" },
        -- { "<leader>sr", "<cmd>lua require('telescope.builtin').resume{}<CR>", desc = "Resume" },
        -- { "<leader>sc", group = "+Commands" },
        -- { "<leader>scr", "<cmd>lua require('telescope.builtin').command_history{}<CR>", desc = "History" },
        -- { "<leader>scc", "<cmd>lua require('telescope.builtin').commands{}<CR>", desc = "Commands" },
        -- { "<leader>sf", group = "+Files" },
        -- {
        --   "<leader>sff",
        --   '<cmd>lua require("telescope.builtin").find_files{ find_command = {"rg", "-i", "--hidden", "--files", "-g", "!.git"} }<CR>',
        --   desc = "Find files",
        -- },
        -- { "<leader>sfg", "<cmd>lua require('telescope.builtin').git_files{}<CR>", desc = "Git files" },
        -- { "<leader>sfj", "<cmd>lua require('telescope.builtin').jumplist{}<CR>", desc = "Jump list" },
        -- { "<leader>sfl", "<cmd>lua require('telescope.builtin').loclist{}<CR>", desc = "Location list" },
        -- { "<leader>sfr", "<cmd>lua require('telescope.builtin').oldfiles{cwd_only = true}<CR>", desc = "Old files" },
        -- { "<leader>sfq", "<cmd>lua require('telescope.builtin').quickfix{}<CR>", desc = "Quickfix" },
        -- { "<leader>sv", group = "+Git" },
        -- { "<leader>svc", "<cmd>lua require('telescope.builtin').git_bcommits{}<CR>", desc = "Buffer commits" },
        -- { "<leader>svC", "<cmd>lua require('telescope.builtin').git_commits{}<CR>", desc = "Commits" },
        -- { "<leader>svs", "<cmd>lua require('telescope.builtin').git_status{}<CR>", desc = "Status" },
        -- { "<leader>svb", "<cmd>lua require('telescope.builtin').git_branches{}<CR>", desc = "Branch" },
        -- { "<leader>sg", group = "+Grep" },
        -- {
        --   "<leader>sgg",
        --   '<cmd>lua require("telescope.builtin").live_grep{ glob_pattern = "!.git" }<CR>',
        --   desc = "Live grep",
        -- },
        -- { "<leader>sgc", "<cmd>lua require('telescope.builtin').grep_string{}<CR>", desc = "Grep string" },

        { "<leader>T", group = "+Task" },
        { "<leader>Tt", "<cmd>OverseerToggle<cr>", desc = "Toggle tasks" },
        { "<leader>Tr", "<cmd>OverseerRun<cr>", desc = "Run task" },
        { "<leader>TR", "<cmd>OverseerRestartLast<cr>", desc = "Restart task" },
        { "<leader>Ti", "<cmd>OverseerInfo<cr>", desc = "Open information" },
        { "<leader>Tc", "<cmd>OverseerRunCmd<cr>", desc = "Run command" },

        { "<leader>g", group = "+Git" },
        { "<leader>gs", "<cmd>lua require('neogit').open({ kind = 'auto' })<cr>", desc = "Status" },
        { "<leader>gd", group = "+Diff" },
        { "<leader>gs", group = "+Stage", mode = "x" },
        { "<leader>gss", ":Gitsigns stage_hunk<cr>", desc = "Select stage", mode = "x" },
        { "<leader>gsu", ":Gitsigns unstage_hunk<cr>", desc = "Select unstage", mode = "x" },
        { "<leader>gdd", "<cmd>Gitsigns diffthis<cr>", desc = "Diff" },
        { "<leader>gL", group = "+Linker" },
        { "<leader>gLc", ":GitLinker current<cr>", desc = "Current git link", mode = { "n", "x" } },
        { "<leader>gLd", ":GitLinker default<cr>", desc = "Default branch git link", mode = { "n", "x" } },
        { "<leader>c", group = "+Comment" },
        { "<leader>cg", group = "+Generate" },
        {
          "<leader>cgf",
          '<cmd>lua require("neogen").generate { type = "func" }<CR>',
          desc = "Generate doc comment for function",
        },
        {
          "<leader>cgF",
          '<cmd>lua require("neogen").generate { type = "file" }<CR>',
          desc = "Generate doc comment for file",
        },
        {
          "<leader>cgt",
          '<cmd>lua require("neogen").generate { type = "type" }<CR>',
          desc = "Generate doc comment for type",
        },
        {
          "<leader>cgc",
          '<cmd>lua require("neogen").generate { type = "class" }<CR>',
          desc = "Generate doc comment for class",
        },
      })
      if vim.g.test_runner_type == "neotest" then
        wk.add({
          mode = "n",
          { "<leader>t", group = "+Test" },
          { "<leader>tr", group = "+Run" },
          { "<leader>to", group = "+Display" },
          { "<leader>trn", '<cmd>lua require("neotest").run.run()<CR>', desc = "Nearest" },
          { "<leader>trf", '<cmd>lua require("neotest").run.run(vim.fn.expand("%"))<CR>', desc = "Current file" },
          { "<leader>trr", '<cmd>lua require("neotest").run.run_last()<CR>', desc = "Last" },
          { "<leader>trs", '<cmd>lua require("neotest").run.stop()<CR>', desc = "Stop" },
          { "<leader>tra", '<cmd>lua require("neotest").run.attach()<CR>', desc = "Attach" },
          { "<leader>tos", '<cmd>lua require("neotest").summary.toggle()<CR>', desc = "Toggle summary" },
          { "<leader>too", '<cmd>lua require("neotest").output.open({ enter = true })<CR>', desc = "Open output" },
          { "<leader>too", '<cmd>lua require("neotest").output_panel.toggle()<CR>', desc = "Toggle output panel" },
        })
      elseif vim.g.test_runner_type == "vim-test" then
        wk.add({
          mode = "n",
          { "<leader>t", group = "+Test" },
          { "<leader>tr", group = "+Run" },
          { "<leader>trn", "<cmd>TestNearest<cr>", desc = "Nearest" },
          { "<leader>trf", "<cmd>TestFile<cr>", desc = "Current file" },
          { "<leader>trr", "<cmd>TestLast<cr>", desc = "Last" },
          { "<leader>trs", "<cmd>TestSuite<cr>", desc = "Suit" },
        })
      end

      if vim.g.file_explorer_type == "neo-tree" then
        wk.add({
          mode = "n",
          { "<leader>e", "<cmd>Neotree toggle<cr>", desc = "File Explorer" },
          { "<leader>E", "<cmd>Neotree reveal<cr>", desc = "File Explorer(focus)" },
        })
      end

      if vim.g.lsp_client_type == "neovim" then
        wk.add({
          mode = "n",
          { "g", group = "+LSP" },
          -- { "gr", "<cmd>Telescope lsp_references<CR>", desc = "References" },
          -- { "gi", "<cmd>Telescope lsp_implementations<CR>", desc = "Implementations" },
          -- { "gd", "<cmd>Telescope lsp_definitions<CR>", desc = "Definitions" },
          -- { "gD", "<cmd>Telescope lsp_type_definitions<CR>", desc = "Type definitions" },
          { "gf", group = "+Go To Definition" },
          { "gff", "<cmd>lua require('gtd').exec({ command = 'edit' })<CR>", desc = "Go to edit" },
          { "gfs", "<cmd>lua require('gtd').exec({ command = 'split' })<CR>", desc = "Go to split" },
          { "gfv", "<cmd>lua require('gtd').exec({ command = 'vsplit' })<CR>", desc = "Go to vsplit" },
          { "gh", group = "+Inlay Hint" },
          { "ght", "<cmd>lua vim.lsp.inlay_hint(0)<CR>", desc = "Toggle" },
          {
            "K",
            function()
              local buf = vim.api.nvim_get_current_buf()
              local ft = vim.api.nvim_get_option_value("filetype", { buf = buf })
              if ft == "rust" then
                return require("rust-tools").hover_actions.hover_actions()
              end
              return vim.lsp.buf.hover()
            end,
            desc = "Hover Doc",
          },
          { "H", "<cmd>lua vim.lsp.buf.signature_help()<CR>", desc = "Signature Help" },
          { "<leader>ac", "<cmd>lua vim.lsp.buf.code_action()<CR>", desc = "Code Action", mode = { "n", "v" } },
          { "<leader>d", group = "+Diagnostics" },
          {
            "<leader>dt",
            (function()
              local is_show = true
              return function()
                if is_show then
                  is_show = false
                  vim.diagnostic.hide()
                else
                  is_show = true
                  vim.diagnostic.show()
                end
              end
            end)(),
            desc = "Toggle diagnostics",
          },
          { "<leader>dc", "<cmd>lua vim.diagnostic.open_float()<CR>", desc = "Open float" },
          { "<leader>do", "<cmd>lua vim.diagnostic.setloclist()<CR>", desc = "Set loclist" },
          { "<leader>dn", "<cmd>lua vim.diagnostic.goto_next()<CR>", desc = "Jump next" },
          { "<leader>dp", "<cmd>lua vim.diagnostic.goto_prev()<CR>", desc = "Jump previous" },
          { "<leader>dp", "<cmd>lua vim.diagnostic.goto_prev()<CR>", desc = "Jump previous" },
          { "<leader>dd", group = "+List" },
          { "<leader>ddd", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Document Diagnostics(Trouble)" },
          { "<leader>ddD", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Workspace Diagnostics(Trouble)" },
          { "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", desc = "Rename" },
        })
      elseif vim.g.lsp_client_type == "coc" then
        wk.add({
          { "<leader>d", group = "+Diagnostics" },
          { "<leader>dd", group = "+List" },
          { "<leader>ddd", "<cmd>Telescope coc diagnostics<CR>", desc = "Diagnostics" },
          { "<leader>ddD", "<cmd>Telescope coc workspace_diagnostics<CR>", desc = "Workspace Diagnostics" },
          ["<lesder>sd"] = { "<cmd>Telescope coc diagnostics<CR>", "Diagnostics" },
          ["<lesder>sD"] = { "<cmd>Telescope coc workspace_diagnostics<CR>", "Workspace Diagnostics" },
          { "<leader>ac", "<cmd>Telescope coc code_actions<CR>", desc = "Code Action", mode = { "n", "v" } },
          ["g"] = {
            name = "+LSP",
            { "gr", "<cmd>Telescope coc references<CR>", desc = "References" },
            { "gi", "<cmd>Telescope coc implementations<CR>", desc = "Implementations" },
            { "gy", "<cmd>Telescope coc type_definitions<CR>", desc = "Type Definitions" },
          },
        })
      end
    end,
  },

  -- ColorScheme
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      style = "moon", -- The theme comes in three styles, `storm`, a darker variant `night` and `day`
      light_style = "day", -- The theme is used when the background is set to light
      transparent = true, -- Enable this to disable setting the background color
      terminal_colors = true, -- Configure the colors used when opening a `:terminal` in Neovim
      styles = {
        -- Style to be applied to different syntax groups
        -- Value is any valid attr-list value for `:help nvim_set_hl`
        comments = { italic = true },
        keywords = { italic = true },
        -- Background styles. Can be "dark", "transparent" or "normal"
        sidebars = "dark", -- style for sidebars, see below
        floats = "dark", -- style for floating windows
      },
      day_brightness = 0.3, -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors
      dim_inactive = false, -- dims inactive windows
      lualine_bold = true, -- When `true`, section headers in the lualine theme will be bold

      cache = true, -- When set to true, the theme will be cached for better performance
    },
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
    "sainnhe/gruvbox-material",
    enabled = false,
    lazy = false,
    priority = 1000,
    config = function()
      -- Optionally configure and load the colorscheme
      -- directly inside the plugin declaration.
      vim.g.gruvbox_material_enable_italic = true
      vim.g.gruvbox_material_background = "soft"
      vim.gruvbox_material_better_performance = 1
    end,
  },

  -- Local plugins
  {
    dir = vim.fs.joinpath(vim.fn.stdpath("config"), "local_plugin", "cspell_manager"),
    cmd = { "CspellAddWordLocal", "CspellAddWordManaged" },
    enabled = function()
      return vim.g.lsp_client_type == "neovim"
    end,
    config = function()
      require("cspell_manager").setup()
    end,
  },
  {
    dir = vim.fs.joinpath(vim.fn.stdpath("config"), "local_plugin", "commit_msg_generator"),
    cmd = { "CommitMsgGen" },
    config = function()
      require("commit_msg_generator").setup({})
    end,
  },
  {
    dir = vim.fs.joinpath(vim.fn.stdpath("config"), "local_plugin", "gen_gitignore"),
    cmd = { "Gigi" },
    config = function()
      require("gen_gitignore").setup()
    end,
  },
  {
    dir = vim.fs.joinpath(vim.fn.stdpath("config"), "local_plugin", "git_linker"),
    cmd = { "GitLinker" },
    config = function()
      require("git_linker").setup()
    end,
  },
  {
    dir = vim.fs.joinpath(vim.fn.stdpath("config"), "local_plugin", "random_colorscheme"),
    lazy = false,
    priority = 10000,
    config = function()
      require("random_colorscheme").setup({
        "edge",
        "tokyonight-moon",
      })
    end,
  },
  {
    "tkmpypy/chowcho.nvim",
    dev = true,
    config = function()
      require("chowcho").setup({
        labels = { "A", "B", "C", "D", "E", "F", "G", "H", "I" },
        selector_style = "float",
        ignore_case = true,
        use_exclude_default = true,
        -- exclude = function(buf, win)
        --   -- exclude noice.nvim's cmdline_popup
        --   local bt = vim.api.nvim_get_option_value("buftype", { buf = buf })
        --   local ft = vim.api.nvim_get_option_value("filetype", { buf = buf })
        --   if bt == "nofile" and (ft == "noice" or ft == "vim") then
        --     return true
        --   end
        --   return false
        -- end,
        selector = {
          float = {
            border_style = "rounded",
            icon_enabled = true,
            color = {
              label = {
                active = "#c8cfff",
                inactive = "#ababab",
              },
              text = {
                active = "#fefefe",
                inactive = "#d0d0d0",
              },
              border = {
                active = "#b400c8",
                inactive = "#fefefe",
              },
            },
            zindex = 1,
          },
          statusline = {
            color = {
              label = {
                active = "#fefefe",
                inactive = "#d0d0d0",
              },
              background = {
                active = "#3d7172",
                inactive = "#203a3a",
              },
            },
          },
        },
      })
    end,
  },
}, {
  defaults = {
    lazy = false, -- should plugins be lazy-loaded?
  },
  dev = {
    path = "~/ghq/github.com/tkmpypy",
    patterns = {},
    fallback = false,
  },
  readme = {
    enabled = true,
    root = vim.fn.stdpath("state") .. "/lazy/readme",
    files = { "README.md", "lua/**/README.md" },
    -- only generate markdown helptags for plugins that dont have docs
    skip_if_doc_exists = true,
  },
  concurrency = 50,
  ui = {
    border = "rounded",
  },
  checker = {
    enabled = false,
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
      },
    },
  },
})
