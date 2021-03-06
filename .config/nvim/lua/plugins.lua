local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({
    'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path
  })
end

execute 'packadd packer.nvim'
local packer = require('packer')
local util = require('packer.util')

packer.startup({
  function(use)
    -- Packer can manage itself as an optional plugin
    use {'wbthomason/packer.nvim'}

    if (vim.g.use_treesitter) then use {'nvim-treesitter/nvim-treesitter'} end

    -- ColorScheme
    use {'altercation/vim-colors-solarized', opt = true}
    use {'Rigellute/rigel', opt = true}
    use {'w0ng/vim-hybrid', opt = true}
    use {'flrnprz/plastic.vim', opt = true}
    use {'KeitaNakamura/neodark.vim', opt = true}
    use {'sonph/onehalf', opt = true}
    use {'cocopon/iceberg.vim', opt = true}
    use {'dracula/vim', opt = true, as = 'dracula'}
    use {'morhetz/gruvbox', opt = true}
    use {'kaicataldo/material.vim', opt = true}
    use {'sainnhe/gruvbox-material'}
    use {'arcticicestudio/nord-vim', opt = true}
    use {'whatyouhide/vim-gotham', opt = true}
    use {'yuttie/hydrangea-vim', opt = true}
    use {'NLKNguyen/papercolor-theme', opt = true}
    use {'junegunn/seoul256.vim', opt = true}
    use {'drewtempelmeyer/palenight.vim'}
    use {'franbach/miramare', opt = true}
    use {'embark-theme/vim', opt = true, as = 'embark'}

    -- supported treesitter colorscheme
    use {'mhartington/oceanic-next', opt = true}
    use {'sainnhe/edge'}
    use {
      'glepnir/zephyr-nvim',
      requires = {{'nvim-treesitter/nvim-treesitter'}}
    }
    use {'savq/melange', opt = true}
    use {'folke/tokyonight.nvim'}
    use {
      'Th3Whit3Wolf/space-nvim',
      config = function() vim.g.space_nvim_transparent_bg = true end
    }
    use {
      'eddyekofo94/gruvbox-flat.nvim',
      config = function()
        vim.g.gruvbox_flat_style = "dark"
        vim.g.gruvbox_italic_functions = true
        vim.g.gruvbox_italic_comments = true
        vim.g.gruvbox_italic_keywords = true
        vim.g.gruvbox_italic_variables = false
        vim.g.gruvbox_transparent = false
        vim.g.gruvbox_dark_sidebar = true
        vim.g.gruvbox_dark_float = true
        vim.g.gruvbox_sidebars = {"qf", "vista_kind", "terminal", "packer"}
        vim.g.gruvbox_hide_inactive_statusline = true
      end
    }
    use {
      'NTBBloodbath/doom-one.nvim',
      config = function()
        vim.g.doom_one_enable_treesitter = true
        vim.g.doom_one_terminal_colors = false
        vim.g.doom_one_transparent_background = false
        vim.g.doom_one_cursor_coloring = true
      end
    }

    -- Languages
    use {'plasticboy/vim-markdown', ft = {'markdown'}}
    use {'euclidianAce/BetterLua.vim', ft = {'lua'}}
    use {
      'towolf/vim-helm',
      config = function()
        vim.cmd [[autocmd BufRead,BufNewFile */templates/*.yml,*/templates/*.yaml,*/templates/*.tpl set ft=helm]]
      end
    }
    use {'aklt/plantuml-syntax', ft = {'plantuml'}}
    use {
      'leafgarland/typescript-vim',
      config = function() vim.g.typescript_indent_disable = true end
    }

    -- runner
    use {'metakirby5/codi.vim'}
    use {'vim-test/vim-test'}
    use {'thinca/vim-quickrun'}

    -- UI
    use {
      'p00f/nvim-ts-rainbow',
      config = function()
        require'nvim-treesitter.configs'.setup {
          rainbow = {
            enable = true,
            extended_mode = true -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
          }
        }
      end
    }
    use {
      'sunjon/shade.nvim',
      disable = true,
      config = function()
        require'shade'.setup({
          overlay_opacity = 50,
          opacity_step = 1,
          keys = {brightness_up = '<C-Up>', brightness_down = '<C-Down>'}
        })
      end
    }

    use {
      'folke/which-key.nvim',
      disable = true,
      config = function() require('which_key.lua') end
    }

    use {'tjdevries/cyclist.vim'}
    use {'norcalli/nvim-colorizer.lua'}
    use {'kyazdani42/nvim-web-devicons'}
    use {
      'glepnir/galaxyline.nvim',
      branch = 'main',
      config = function() return require('statusline') end,
      requires = {'kyazdani42/nvim-web-devicons'}
    }
    use {
      'akinsho/nvim-bufferline.lua',
      requires = {'kyazdani42/nvim-web-devicons'},
      config = function()
        require'bufferline'.setup {
          options = {
            view = "multiwindow", -- "multiwindow" | "default"
            numbers = "ordinal", -- "none" | "ordinal" | "buffer_id"
            number_style = "superscript",
            mappings = true,
            buffer_close_icon = '',
            modified_icon = '●',
            close_icon = '',
            left_trunc_marker = '',
            right_trunc_marker = '',
            max_name_length = 18,
            tab_size = 18,
            show_buffer_close_icons = true,
            -- can also be a table containing 2 custom separators
            -- [focused and unfocused]. eg: { '|', '|' }
            separator_style = "slant", -- "slant" | "thick" | "thin" | { 'any', 'any' }
            enforce_regular_tabs = false,
            always_show_bufferline = true,
            sort_by = 'extension'
          }
        }
      end
    }
    use {'mhinz/vim-startify'}
    use {'liuchengxu/vista.vim'}

    -- tree
    use {
      'kyazdani42/nvim-tree.lua',
      config = function()
        local tree_cb = require'nvim-tree.config'.nvim_tree_callback
        vim.g.nvim_tree_side = 'left' -- left by default
        vim.g.nvim_tree_width = 30 -- 30 by default
        vim.g.nvim_tree_ignore = {'node_modules', '.cache', '.DS_Store'} -- empty by default
        vim.g.nvim_tree_auto_open = 0 -- 0 by default, opens the tree when typing `vim $DIR` or `vim`

        vim.g.nvim_tree_auto_close = 0 -- 0 by default, closes the tree when it's the last window
        vim.g.nvim_tree_follow = 0 -- 0 by default, this option allows the cursor to be updated when entering a buffer
        vim.g.nvim_tree_indent_markers = 1 -- 0 by default, this option shows indent markers when folders are open
        vim.g.nvim_tree_hide_dotfiles = 0 -- 0 by default, this option hides files and folders starting with a dot `.`
        vim.g.nvim_tree_git_hl = 1 -- 0 by default, will enable file highlight for git attributes (can be used without the icons).
        vim.g.nvim_tree_root_folder_modifier = ':~' -- This is the default. See :help filename-modifiers for more options
        vim.g.nvim_tree_tab_open = 0 -- 0 by default, will open the tree when entering a new tab and the tree was previously open
        vim.g.nvim_tree_show_icons = {
          git = 1,
          folders = 1,
          files = 1,
          folder_arrows = 1
        }
        vim.g.nvim_tree_disable_default_keybindings = 0
        vim.g.nvim_tree_bindings = {
          {key = "n", mode = "n", cb = tree_cb("create")},
          {key = "u", mode = "n", cb = tree_cb("dir_up")},
          {key = {"<CR>", "o", "<2-LeftMouse>"}, cb = tree_cb("edit")},
          {key = {"<2-RightMouse>", "<C-}>"}, cb = tree_cb("cd")},
          {key = "<C-v>", cb = tree_cb("vsplit")},
          {key = "<C-x>", cb = tree_cb("split")},
          {key = "<C-t>", cb = tree_cb("tabnew")},
          {key = "<", cb = tree_cb("prev_sibling")},
          {key = ">", cb = tree_cb("next_sibling")},
          {key = "P", cb = tree_cb("parent_node")},
          {key = "<BS>", cb = tree_cb("close_node")},
          {key = "<S-CR>", cb = tree_cb("close_node")},
          {key = "<Tab>", cb = tree_cb("preview")},
          {key = "K", cb = tree_cb("first_sibling")},
          {key = "J", cb = tree_cb("last_sibling")},
          {key = "I", cb = tree_cb("toggle_ignored")},
          {key = "H", cb = tree_cb("toggle_dotfiles")},
          {key = "R", cb = tree_cb("refresh")},
          {key = "d", cb = tree_cb("remove")},
          {key = "r", cb = tree_cb("rename")},
          {key = "<C->", cb = tree_cb("full_rename")},
          {key = "x", cb = tree_cb("cut")}, {key = "c", cb = tree_cb("copy")},
          {key = "p", cb = tree_cb("paste")},
          {key = "y", cb = tree_cb("copy_name")},
          {key = "Y", cb = tree_cb("copy_path")},
          {key = "gy", cb = tree_cb("copy_absolute_path")},
          {key = "[c", cb = tree_cb("prev_git_item")},
          {key = "}c", cb = tree_cb("next_git_item")},
          {key = "-", cb = tree_cb("dir_up")},
          {key = "q", cb = tree_cb("close")},
          {key = "g?", cb = tree_cb("toggle_help")}
        }
        vim.api.nvim_set_keymap('n', '<Leader>ft', ":NvimTreeToggle<CR>", {})
        vim.api.nvim_set_keymap('n', '<Leader>fr', ":NvimTreeRefresh<CR>", {})
        vim.api.nvim_set_keymap('n', '<Leader>ff', ":NvimTreeFindFile<CR>", {})
      end
    }

    -- Lua Utils
    use {'rafcamlet/nvim-luapad'}

    -- Utils
    use {'itchyny/vim-winfix'}
    use {
      'akinsho/nvim-toggleterm.lua',
      config = function()
        require"toggleterm".setup {
          size = 20,
          open_mapping = [[<c-\>]],
          shade_filetypes = {},
          shade_terminals = true,
          direction = 'horizontal'
        }
      end
    }
    use {'moll/vim-bbye'}
    -- use {'tyru/caw.vim'}
    use {
      'b3nj5m1n/kommentary',
      config = function()
        local config = require('kommentary.config')
        config.use_extended_mappings()
        config.configure_language("default", {
          ignore_whitespace = true,
          use_consistent_indentation = true,
          prefer_single_line_comments = true,
          hook_function = function()
            require('ts_context_commentstring.internal').update_commentstring()
          end
        })
      end
    }
    use {
      'JoosepAlviste/nvim-ts-context-commentstring',
      config = function()
        require'nvim-treesitter.configs'.setup {
          context_commentstring = {enable = true, enable_autocmd = false}
        }
      end
    }

    use {'godlygeek/tabular'}
    use {'lukas-reineke/format.nvim'}
    use {'airblade/vim-rooter'}
    use {'machakann/vim-sandwich'}
    use {'simeji/winresizer'}
    -- use {'cohama/lexima.vim'}
    use {
      'windwp/nvim-autopairs',
      config = function()
        require('nvim-autopairs').setup({
          disable_filetype = {"TelescopePrompt"},
          ignored_next_char = string.gsub([[ [%w%%%'%[%"%.] ]], "%s+", ""),
          enable_moveright = true,
          enable_afterquote = true, -- add bracket pairs after quote
          enable_check_bracket_line = true, --- check bracket in same line
          check_ts = false
        })
        if (vim.g.lsp_client_type == 'neovim') then
          require("nvim-autopairs.completion.compe").setup(
              {
                map_cr = true, --  map <CR> on insert mode
                map_complete = true -- it will auto insert `(` after select function or method item
              })
        end
      end
    }
    use {'iamcco/markdown-preview.nvim', run = 'cd app && yarn install'}
    use {'npxbr/glow.nvim'}
    use {'glidenote/memolist.vim'}
    use {'mbbill/undotree'}
    use {'osyo-manga/vim-over'}
    use {'tyru/operator-camelize.vim', requires = {{'kana/vim-operator-user'}}}
    use {'pechorin/any-jump.vim'}
    use {'hrsh7th/vim-eft'}
    use {
      'phaazon/hop.nvim',
      config = function()
        vim.api.nvim_set_keymap('n', ',', ":HopWord<cr>", {})
      end
    }
    use {'mtdl9/vim-log-highlighting', opt = true}
    use {'tversteeg/registers.nvim'}
    -- use {'gelguy/wilder.nvim', run = ':UpdateRemotePlugins'}
    use {'bfredl/nvim-miniyank'}
    use {
      'kristijanhusak/orgmode.nvim',
      config = function()
        require('orgmode').setup({
          org_agenda_files = {'~/Dropbox/org/*'},
          org_default_notes_file = '~/Dropbox/org/note.org',
          org_todo_keywords = {'TODO', 'DOING', 'HOLD', '|', 'DONE'},
          org_agenda_templates = {
            t = {
              description = 'Task',
              template = '* TODO %?\n  %u',
              target = '~/Dropbox/org/todo.org'
            },
            n = {
              description = 'Note',
              template = '* NOTE %?\n  %u',
              target = '~/Dropbox/org/note.org'
            },
            j = {
              description = 'Journal',
              template = '\n*** %<%Y-%m-%d> %<%A>\n**** %U\n\n%?',
              target = '~/Dropbox/org/journal.org'
            }
          },
          org_deadline_warning_days = 14,
          org_agenda_span = 'week', -- day/week/month/year/number of days
          org_agenda_start_on_weekday = 1,
          org_agenda_start_day = nil, -- start from today + this modifier
          org_priority_highest = 'A',
          org_priority_default = 'B',
          org_priority_lowest = 'C',
          -- org_archive_location = '%s_archive::',
          org_use_tag_inheritance = true,
          -- org_tags_exclude_from_inheritance = {},
          org_hide_leading_stars = false,
          org_hide_emphasis_markers = false,
          org_log_done = 'time',
          mappings = {
            disable_all = false,
            global = {org_agenda = '<Leader>oa', org_capture = '<Leader>oc'},
            agenda = {
              org_agenda_later = 'f',
              org_agenda_earlier = 'b',
              org_agenda_goto_today = '.',
              org_agenda_day_view = 'vd',
              org_agenda_week_view = 'vw',
              org_agenda_month_view = 'vm',
              org_agenda_year_view = 'vy',
              org_agenda_quit = 'q',
              org_agenda_switch_to = '<CR>',
              org_agenda_goto = {'<TAB>'},
              org_agenda_goto_date = 'J',
              org_agenda_redo = 'r',
              org_agenda_show_help = '?'
            },
            capture = {
              org_capture_finalize = '<C-c>',
              org_capture_refile = '<Leader>or',
              org_capture_kill = '<Leader>ok',
              org_capture_show_help = '?'
            },
            org = {
              org_refile = '<Leader>or',
              org_increase_date = '<Leader>da',
              org_decrease_date = '<Leader>dx',
              org_change_date = 'cid',
              org_todo = 'cit',
              org_todo_prev = 'ciT',
              org_toggle_checkbox = '<Leader>oT',
              org_open_at_point = '<Leader>oo',
              org_cycle = '<TAB>',
              org_global_cycle = '<S-TAB>',
              org_archive_subtree = '<Leader>o$',
              org_set_tags_command = '<Leader>t',
              org_toggle_archive_tag = '<Leader>oA',
              org_do_promote = '<<',
              org_do_demote = '>>',
              org_promote_subtree = '<s',
              org_demote_subtree = '>s',
              org_meta_return = '<Leader><CR>', -- Add headling, item or row
              org_insert_heading_respect_content = '<Leader>oih', -- Add new headling after current heading block with same level
              org_insert_todo_heading = '<Leader>oiT', -- Add new todo headling right after current heading with same level
              org_insert_todo_heading_respect_content = '<Leader>oit', -- Add new todo headling after current heading block on same level
              org_move_subtree_up = '<Leader>oK',
              org_move_subtree_down = '<Leader>oJ',
              org_show_help = '?'
            }
          }
        })
        vim.api.nvim_set_keymap("n", "<Leader>on",
                                "<cmd>:e ~/Dropbox/org/note.org<cr>",
                                {silent = true, noremap = true})
        vim.api.nvim_set_keymap("n", "<Leader>ot",
                                "<cmd>:e ~/Dropbox/org/todo.org<cr>",
                                {silent = true, noremap = true})
        vim.api.nvim_set_keymap("n", "<Leader>oj",
                                "<cmd>:e ~/Dropbox/org/journal.org<cr>",
                                {silent = true, noremap = true})
      end
    }

    -- finder

    if (vim.g.lsp_client_type == 'coc') then

      use {'fannheyward/telescope-coc.nvim'}
    end

    use {
      'nvim-telescope/telescope.nvim',
      requires = {
        {'nvim-lua/plenary.nvim'}, {'nvim-lua/popup.nvim'},
        {'tkmpypy/telescope-jumps.nvim'}
      },
      config = function()
        local telescope = require('telescope')
        telescope.load_extension('jumps')
        if (vim.g.lsp_client_type == 'coc') then
          telescope.load_extension('coc')
        end
        telescope.setup {
          defaults = {
            vimgrep_arguments = {
              'rg', '--color=never', '--no-heading', '--with-filename',
              '--line-number', '--column', '--smart-case', '--hidden'
            },
            shortlen_path = true,
            winblend = 10,
            scroll_strategy = 'cycle',
            color_devicon = true
          },
          pickers = {
            buffers = {
              sort_lastused = true,
              theme = "dropdown",
              -- previewer = true,
              mappings = {
                i = {["<c-d>"] = require("telescope.actions").delete_buffer},
                n = {["<c-d>"] = require("telescope.actions").delete_buffer}
              }
            }
          }
        }
      end
    }
    use {
      "folke/todo-comments.nvim",
      requires = {'nvim-telescope/telescope.nvim'},
      config = function()
        require("todo-comments").setup {
          -- pattern = "(KEYWORDS)[(.*)]?.*:"
          highlight = {
            before = "", -- "fg" or "bg" or empty
            keyword = "wide", -- "fg", "bg", "wide" or empty. (wide is the same as bg, but will also highlight surrounding characters)
            after = "fg", -- "fg" or "bg" or empty
            pattern = [[.*<(KEYWORDS)\s*:]], -- pattern used for highlightng (vim regex)
            comments_only = true -- this applies the pattern only inside comments using `commentstring` option
          },
          -- list of named colors where we try to extract the guifg from the
          -- list of hilight groups or use the hex color if hl not found as a fallback
          colors = {
            error = {"LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626"},
            warning = {"LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24"},
            info = {"LspDiagnosticsDefaultInformation", "#2563EB"},
            hint = {"LspDiagnosticsDefaultHint", "#10B981"},
            default = {"Identifier", "#7C3AED"}
          },
          search = {
            command = "rg",
            args = {
              "--color=never", "--no-heading", "--with-filename",
              "--line-number", "--column"
            },
            -- regex that will be used to match keywords.
            -- don't replace the (KEYWORDS) placeholder
            pattern = [[\b(KEYWORDS):]] -- ripgrep regex
            -- pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You'll likely get false positives
          }
        }

        vim.api.nvim_set_keymap("n", "<leader>st", "<cmd>TodoTelescope<cr>",
                                {silent = true, noremap = true})
      end
    }

    -- Git
    use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'}}
    use {'lambdalisue/gina.vim'}
    use {'rhysd/git-messenger.vim'}
    use {'APZelos/blamer.nvim'}
    use {
      'pwntester/octo.nvim',
      config = function()
        require"octo".setup({
          date_format = "%Y %b %d %I:%M %p %Z", -- date format
          default_remote = {"upstream", "origin"}, -- order to try remotes
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
            use_icons = true -- use web-devicons in file panel
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
              react_confused = "<space>rc" -- add/remove 😕 reaction
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
              react_confused = "<space>rc" -- add/remove 😕 reaction
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
              react_confused = "<space>rc" -- add/remove 😕 reaction
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
              close_review_tab = "<C-c>" -- close review tab
            },
            submit_win = {
              approve_review = "<C-a>", -- approve review
              comment_review = "<C-m>", -- comment review
              request_changes = "<C-r>", -- request changes review
              close_review_tab = "<C-c>" -- close review tab
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
              close_review_tab = "<C-c>" -- close review tab
            }
          }
        })
        --[[ vim.api.nvim_set_keymap("n", "<leader>gil",
                          "<cmd>Octo issue list<cr>",
                          {silent = true, noremap = true})
      vim.api.nvim_set_keymap("n", "<leader>gpl",
                          "<cmd>Octo pr list<cr>",
                          {silent = true, noremap = true}) ]]

      end
    }

    -- Snippet
    if (vim.g.lsp_client_type == 'neovim') then
      -- use neovim built-in
      use {'neovim/nvim-lspconfig'}
      use {'kabouzeid/nvim-lspinstall'}
      use {'nvim-lua/lsp-status.nvim', disable = true}
      use {'tjdevries/lsp_extensions.nvim', disable = true}
      use {'hrsh7th/vim-vsnip'}
      use {'hrsh7th/vim-vsnip-integ', requires = {{'hrsh7th/vim-vsnip'}}}
      use {
        'hrsh7th/nvim-compe',
        requires = {{'hrsh7th/vim-vsnip'}},
        config = function()
          require'compe'.setup {
            enabled = true,
            autocomplete = true,
            debug = false,
            min_length = 1,
            preselect = 'enable',
            throttle_time = 80,
            source_timeout = 200,
            resolve_timeout = 800,
            incomplete_delay = 400,
            max_abbr_width = 100,
            max_kind_width = 100,
            max_menu_width = 100,
            documentation = {
              border = {'', '', '', ' ', '', '', '', ' '}, -- the border option is the same as `|help nvim_open_win|`
              winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
              max_width = 120,
              min_width = 60,
              max_height = math.floor(vim.o.lines * 0.3),
              min_height = 1
            },
            source = {
              orgmode = true,
              path = {priority = 11},
              buffer = {priority = 10},
              nvim_lsp = {priority = 15},
              nvim_lua = {priority = 90},
              vsnip = {priority = 14}
            }
          }
        end
      }
      use {
        'onsails/lspkind-nvim',
        config = function()
          require('lspkind').init({
            with_text = false,
            symbol_map = {
              Text = '',
              Method = 'ƒ',
              Function = '',
              Constructor = '',
              Variable = '',
              Class = '',
              Interface = 'ﰮ',
              Module = '',
              Property = '',
              Unit = '',
              Value = '',
              Enum = '了',
              Keyword = '',
              Snippet = '﬌',
              Color = '',
              File = '',
              Folder = '',
              EnumMember = '',
              Constant = '',
              Struct = ''
            }
          })
        end
      }

      use {
        'glepnir/lspsaga.nvim',
        require = {{'neovim/nvim-lspconfig'}},
        config = function()
          local saga = require('lspsaga')
          saga.init_lsp_saga {
            -- add your config value here
            -- default value
            use_saga_diagnostic_sign = false
            -- error_sign = '',
            -- warn_sign = '',
            -- hint_sign = '',
            -- infor_sign = '',
            -- dianostic_header_icon = '   ',
            -- code_action_icon = ' ',
            -- code_action_prompt = {
            --   enable = true,
            --   sign = true,
            --   sign_priority = 20,
            --   virtual_text = true,
            -- },
            -- finder_definition_icon = '  ',
            -- finder_reference_icon = '  ',
            -- max_preview_lines = 10, -- preview lines of lsp_finder and definition preview
            -- finder_action_keys = {
            --   open = 'o', vsplit = 's',split = 'i',quit = 'q',scroll_down = '<C-f>', scroll_up = '<C-b>' -- quit can be a table
            -- },
            -- code_action_keys = {
            --   quit = 'q',exec = '<CR>'
            -- },
            -- rename_action_keys = {
            --   quit = '<C-c>',exec = '<CR>'  -- quit can be a table
            -- },
            -- definition_preview_icon = '  '
            -- "single" "double" "round" "plus"
            -- border_style = "single"
            -- rename_prompt_prefix = '➤',
            -- if you don't use nvim-lspconfig you must pass your server name and
            -- the related filetypes into this table
            -- like server_filetype_map = {metals = {'sbt', 'scala'}}
            -- server_filetype_map = {}
          }
        end
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
            mode = "document", -- "workspace" or "document"
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
              next = "j" -- next item
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
              information = ""
            },
            use_lsp_diagnostic_signs = false -- enabling this will use the signs defined in your lsp client
          }

          vim.api.nvim_set_keymap("n", "<leader>sd",
                                  "<cmd>LspTroubleDocumentToggle<cr>",
                                  {silent = true, noremap = true})
          vim.api.nvim_set_keymap("n", "<leader>sD",
                                  "<cmd>LspTroubleWorkspaceToggle<cr>",
                                  {silent = true, noremap = true})
        end
      }

      use {
        "akinsho/flutter-tools.nvim",
        requires = {"neovim/nvim-lspconfig"},
        ft = {'dart'}
      }
    elseif (vim.g.lsp_client_type == 'coc') then
      use {
        'neoclide/coc.nvim',
        branch = 'master',
        run = 'yarn install --frozen-lockfile',
        requires = {'rafcamlet/coc-nvim-lua'}
      }
    end

    use {
      '~/private/chowcho.nvim',
      config = function()
        require('chowcho').setup {border_style = 'rounded', icon_enabled = true}
      end
    }

    use {
      disable = true,
      '~/private/akari.nvim',
      config = function() require('akari').setup({debug = false}) end
    }
  end,
  config = {display = {open_fn = util.float}}
})

