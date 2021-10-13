local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
local packer_compiled_path = fn.stdpath("config") .. "/lua/packer_compiled.lua"

if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({
		"git",
		"clone",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
end

local packer = require("packer")
local packer_util = require("packer.util")

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
    autocmd User PackerCompileDone silent! lua require("packer_compiled")
  augroup end
]])

if fn.empty(fn.glob(packer_compiled_path)) == 0 then
	require("packer_compiled")
end

packer.startup({
	function(use)
		-- Packer can manage itself as an optional plugin
		use({ "wbthomason/packer.nvim" })
		use({
			"lewis6991/impatient.nvim",
			config = function()
				require("impatient").enable_profile()
			end,
		})

		if vim.g.use_treesitter then
			use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
		end

		-- ColorScheme
		use({ "Rigellute/rigel", opt = true })
		use({ "arcticicestudio/nord-vim", opt = true })
		use({ "drewtempelmeyer/palenight.vim" })
		use({ "franbach/miramare", opt = true })
		use({ "embark-theme/vim", opt = true, as = "embark" })

		-- supported treesitter colorscheme
		use({ "mhartington/oceanic-next", opt = true })
		use({ "sainnhe/edge" })
		use({
			"sainnhe/everforest",
			config = function()
				-- Set contrast.
				--   This configuration option should be placed before `colorscheme everforest`.
				--   Available values: 'hard', 'medium'(default), 'soft'
				vim.g.everforest_background = "soft"
				vim.g.everforest_enable_italic = 1
				vim.g.everforest_disable_italic_comment = 0
				vim.g.everforest_transparent_background = 1
				vim.g.everforest_ui_contrast = "low" -- high or low
				vim.g.everforest_diagnostic_text_highlight = 1
				vim.g.everforest_diagnostic_line_highlight = 1
			end,
		})
		use({
			"glepnir/zephyr-nvim",
			requires = { { "nvim-treesitter/nvim-treesitter" } },
		})
		use({ "savq/melange", opt = true })
		use({
			"tiagovla/tokyodark.nvim",
			config = function()
				vim.g.tokyodark_transparent_background = false
				vim.g.tokyodark_enable_italic_comment = true
				vim.g.tokyodark_enable_italic = true
				vim.g.tokyodark_color_gamma = "1.5"
			end,
		})
		use({
			"folke/tokyonight.nvim",
			config = function()
				vim.g.tokyonight_style = "storm"
				vim.g.tokyonight_italic_functions = true
			end,
		})
		use({
			"Th3Whit3Wolf/space-nvim",
			config = function()
				vim.g.space_nvim_transparent_bg = true
			end,
		})
		use({
			"eddyekofo94/gruvbox-flat.nvim",
			config = function()
				vim.g.gruvbox_flat_style = "dark"
				vim.g.gruvbox_italic_functions = true
				vim.g.gruvbox_italic_comments = true
				vim.g.gruvbox_italic_keywords = true
				vim.g.gruvbox_italic_variables = false
				vim.g.gruvbox_transparent = false
				vim.g.gruvbox_dark_sidebar = true
				vim.g.gruvbox_dark_float = true
				vim.g.gruvbox_sidebars = { "qf", "vista_kind", "terminal", "packer" }
				vim.g.gruvbox_hide_inactive_statusline = true
			end,
		})
		use({
			"NTBBloodbath/doom-one.nvim",
			disable = true,
			config = function()
				require("doom-one").setup({
					cursor_coloring = false,
					terminal_colors = false,
					italic_comments = false,
					enable_treesitter = true,
					transparent_background = false,
					pumblend = {
						enable = false,
						transparency_amount = 20,
					},
					plugins_integrations = {
						barbar = true,
						bufferline = true,
						gitgutter = true,
						gitsigns = true,
						telescope = true,
						neogit = true,
						nvim_tree = true,
						dashboard = true,
						startify = false,
						whichkey = false,
						indent_blankline = true,
						vim_illuminate = true,
						lspsaga = true,
					},
				})
			end,
		})
		use({
			"EdenEast/nightfox.nvim",
			config = function()
				local nightfox = require("nightfox")
				nightfox.setup({
					fox = "nordfox", -- change the colorscheme to use nordfox
					styles = {
						comments = "italic", -- change style of comments to be italic
						keywords = "bold", -- change style of keywords to be bold
						functions = "italic,bold", -- styles can be a comma separated list
					},
					inverse = {
						match_paren = true, -- inverse the highlighting of match_parens
						visual = false,
						search = false,
					},
				})
				-- nightfox.load()
			end,
		})

		-- Languages
		use({
			"plasticboy/vim-markdown",
			ft = { "markdown" },
			config = function()
				vim.g.vim_markdown_folding_disabled = 0
				vim.g.vim_markdown_conceal = 0
				vim.g.vim_markdown_folding_level = 6
				vim.g.vim_markdown_folding_style_pythonic = 1
			end,
		})
		use({ "hashivim/vim-terraform" })
		use({ "uarun/vim-protobuf", ft = { "proto" } })
		use({ "euclidianAce/BetterLua.vim", ft = { "lua" } })
		use({
			"towolf/vim-helm",
			config = function()
				vim.cmd(
					[[autocmd BufRead,BufNewFile */templates/*.yml,*/templates/*.yaml,*/templates/*.tpl set ft=helm]]
				)
			end,
		})
		use({ "aklt/plantuml-syntax", ft = { "plantuml" } })
		use({
			"leafgarland/typescript-vim",
			config = function()
				vim.g.typescript_indent_disable = true
			end,
		})

		-- runner
		use({ "metakirby5/codi.vim" })
		use({ "vim-test/vim-test" })
		use({ "thinca/vim-quickrun" })

		-- UI
		use({
			"lukas-reineke/indent-blankline.nvim",
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
					enabled = false,
					buftype_exclude = { "terminal", "help" },
					filetype_exclude = { "startify", "alpha" },
					show_end_of_line = false,
					-- space_char_blankline = " ",
					show_trailing_blankline_indent = false,
					-- char_highlight_list = {
					--   "IndentBlanklineIndent1", "IndentBlanklineIndent2"
					-- },
					show_current_context = false,
				})
			end,
		})
		use({
			"mvllow/modes.nvim",
			disable = true,
			config = function()
				require("modes").setup({
					colors = {
						copy = "#f5c359",
						delete = "#c75c6a",
						insert = "#78ccc5",
						visual = "#9745be",
					},
					line_opacity = 0.1,
				})
			end,
		})
		use({
			"p00f/nvim-ts-rainbow",
			config = function()
				require("nvim-treesitter.configs").setup({
					rainbow = {
						enable = true,
						extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
					},
				})
			end,
		})
		use({
			"sunjon/shade.nvim",
			disable = true,
			config = function()
				require("shade").setup({
					overlay_opacity = 50,
					opacity_step = 1,
					keys = { brightness_up = "<C-Up>", brightness_down = "<C-Down>" },
				})
			end,
		})

		use({
			"folke/which-key.nvim",
			disable = true,
			config = function()
				require("which_key.lua")
			end,
		})

		use({ "norcalli/nvim-colorizer.lua" })
		use({ "kyazdani42/nvim-web-devicons" })
		use({
			"famiu/feline.nvim",
			config = function()
				local lsp = require("feline.providers.lsp")
				local vi_mode_utils = require("feline.providers.vi_mode")
				local colors = {
					-- bg = "NONE",
					bg = "#2E3440",
					fg = "NONE",
					fg_green = "#8FBCBB",
					yellow = "#EBCB8B",
					cyan = "#A3BE8C",
					darkblue = "#81A1C1",
					green = "#8FBCBB",
					orange = "#D08770",
					purple = "#B48EAD",
					magenta = "#BF616A",
					gray = "#616E88",
					blue = "#5E81AC",
					red = "#BF616A",
					violet = "#b294bb",
				}
				local vi_mode_colors = {
					["NORMAL"] = "green",
					["OP"] = "green",
					["INSERT"] = "cyan",
					["VISUAL"] = "violet",
					["LINES"] = "violet",
					["BLOCK"] = "violet",
					["REPLACE"] = "red",
					["V-REPLACE"] = "red",
					["ENTER"] = "cyan",
					["MORE"] = "cyan",
					["SELECT"] = "orange",
					["COMMAND"] = "magenta",
					["SHELL"] = "green",
					["TERM"] = "blue",
					["NONE"] = "yellow",
				}
				local mode_alias = {
					["n"] = "NORMAL",
					["no"] = "OP",
					["nov"] = "OP",
					["noV"] = "OP",
					["no"] = "OP",
					["niI"] = "NORMAL",
					["niR"] = "NORMAL",
					["niV"] = "NORMAL",
					["v"] = "VISUAL",
					["V"] = "V-LINES",
					[""] = "V-BLOCK",
					["s"] = "SELECT",
					["S"] = "SELECT",
					[""] = "BLOCK",
					["i"] = "INSERT",
					["ic"] = "INSERT",
					["ix"] = "INSERT",
					["R"] = "REPLACE",
					["Rc"] = "REPLACE",
					["Rv"] = "V-REPLACE",
					["Rx"] = "REPLACE",
					["c"] = "COMMAND",
					["cv"] = "COMMAND",
					["ce"] = "COMMAND",
					["r"] = "ENTER",
					["rm"] = "MORE",
					["r?"] = "CONFIRM",
					["!"] = "SHELL",
					["t"] = "TERM",
					["null"] = "NONE",
				}
				local comps = {
					vi_mode = {
						icon = "",
						provider = function()
							local mode = vim.api.nvim_get_mode().mode
							return " " .. mode_alias[mode] .. " "
						end,
						hl = function()
							return {
								name = vi_mode_utils.get_mode_highlight_name(),
								bg = vi_mode_utils.get_mode_color(),
								fg = "#2c323c",
								style = "bold",
							}
						end,
						right_sep = " ",
					},
					file = {
						info = {
							-- provider = 'file_info',
							provider = function()
								local util = require("scripts.util")
								return util.file.get_current_ufn()
							end,
							hl = {
								fg = colors.blue,
								style = "bold",
							},
							left_sep = " ",
						},
						encoding = {
							provider = "file_encoding",
							left_sep = " ",
							hl = {
								fg = colors.violet,
								style = "bold",
							},
						},
						type = {
							provider = "file_type",
						},
						os = {
							provider = function()
								local util = require("scripts.util")
								return util.os.icon()
							end,
							left_sep = " ",
							hl = {
								fg = colors.violet,
								style = "bold",
							},
						},
					},
					line_percentage = {
						provider = "line_percentage",
						left_sep = " ",
						hl = {
							style = "bold",
						},
					},
					position = {
						provider = "position",
						left_sep = " ",
						hl = {
							style = "bold",
						},
					},
					scroll_bar = {
						provider = "scroll_bar",
						left_sep = " ",
						hl = {
							fg = colors.blue,
							style = "bold",
						},
					},
					diagnos = {
						err = {
							provider = "diagnostic_errors",
							enabled = function()
								return lsp.diagnostics_exist("Error")
							end,
							hl = {
								fg = colors.red,
							},
						},
						warn = {
							provider = "diagnostic_warnings",
							enabled = function()
								return lsp.diagnostics_exist("Warning")
							end,
							hl = {
								fg = colors.yellow,
							},
						},
						hint = {
							provider = "diagnostic_hints",
							enabled = function()
								return lsp.diagnostics_exist("Hint")
							end,
							hl = {
								fg = colors.cyan,
							},
						},
						info = {
							provider = "diagnostic_info",
							enabled = function()
								return lsp.diagnostics_exist("Information")
							end,
							hl = {
								fg = colors.blue,
							},
						},
					},
					lsp = {
						name = {
							provider = "lsp_client_names",
							left_sep = " ",
							icon = " ",
							hl = {
								fg = colors.yellow,
							},
						},
					},
					git = {
						branch = {
							provider = "git_branch",
							icon = " ",
							left_sep = " ",
							hl = {
								fg = colors.violet,
								style = "bold",
							},
						},
						add = {
							provider = "git_diff_added",
							hl = {
								fg = colors.green,
							},
						},
						change = {
							provider = "git_diff_changed",
							hl = {
								fg = colors.orange,
							},
						},
						remove = {
							provider = "git_diff_removed",
							hl = {
								fg = colors.red,
							},
						},
					},
				}
				local properties = {
					force_inactive = {
						filetypes = {
							"NvimTree",
							"dbui",
							"packer",
							"startify",
							"fugitive",
							"fugitiveblame",
						},
						buftypes = { "terminal" },
						bufnames = {},
					},
				}
				local components = {
					active = {
						{
							comps.vi_mode,
							comps.file.info,
							comps.lsp.name,
							comps.diagnos.err,
							comps.diagnos.warn,
							comps.diagnos.hint,
							comps.diagnos.info,
						},
						{},
						{
							comps.git.branch,
							comps.git.add,
							comps.git.change,
							comps.git.remove,
							comps.file.os,
							comps.scroll_bar,
							comps.line_percentage,
							comps.position,
						},
					},
					inactive = {
						{
							comps.file.info,
							comps.file.os,
						},
					},
				}

				require("feline").setup({
					colors = colors,
					components = components,
					force_inactive = properties.force_inactive,
					vi_mode_colors = vi_mode_colors,
				})
			end,
		})
		use({
			"akinsho/bufferline.nvim",
			requires = { "kyazdani42/nvim-web-devicons" },
			config = function()
				require("bufferline").setup({
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
				})
			end,
		})
		use({
			"goolord/alpha-nvim",
			requires = { "kyazdani42/nvim-web-devicons" },
			config = function()
				local alpha = require("alpha")
				local theme = require("alpha.themes.startify")

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
				alpha.setup(theme.opts)
				vim.cmd([[
            autocmd FileType alpha setlocal nofoldenable
        ]])
			end,
		})
		use({ "liuchengxu/vista.vim" })

		-- tree
		use({
			"kyazdani42/nvim-tree.lua",
			config = function()
				local tree_cb = require("nvim-tree.config").nvim_tree_callback
				vim.g.nvim_tree_ignore = { "node_modules", ".cache", ".DS_Store" } -- empty by default
				vim.g.nvim_tree_indent_markers = 1 -- 0 by default, this option shows indent markers when folders are open
				vim.g.nvim_tree_hide_dotfiles = 0 -- 0 by default, this option hides files and folders starting with a dot `.`
				vim.g.nvim_tree_git_hl = 1 -- 0 by default, will enable file highlight for git attributes (can be used without the icons).
				vim.g.nvim_tree_root_folder_modifier = ":~" -- This is the default. See :help filename-modifiers for more options
				vim.g.nvim_tree_show_icons = {
					git = 1,
					folders = 1,
					files = 1,
					folder_arrows = 1,
				}
				vim.api.nvim_set_keymap("n", "<Leader>ft", ":NvimTreeToggle<CR>", {})
				vim.api.nvim_set_keymap("n", "<Leader>fr", ":NvimTreeRefresh<CR>", {})
				vim.api.nvim_set_keymap("n", "<Leader>ff", ":NvimTreeFindFile<CR>", {})
				require("nvim-tree").setup({
					-- disables netrw completely
					disable_netrw = true,
					-- hijack netrw window on startup
					hijack_netrw = true,
					-- open the tree when running this setup function
					open_on_setup = false,
					-- will not open on setup if the filetype is in this list
					ignore_ft_on_setup = {},
					-- closes neovim automatically when the tree is the last **WINDOW** in the view
					auto_close = false,
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
							hint = "",
							info = "",
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

					view = {
						-- width of the window, can be either a number (columns) or a string in `%`
						width = 30,
						-- side of the tree, can be one of 'left' | 'right' | 'top' | 'bottom'
						side = "left",
						-- if true the tree will resize itself after opening a file
						auto_resize = false,
						mappings = {
							-- custom only false will merge the list with the default mappings
							-- if true, it will only use your list to set the mappings
							custom_only = true,
							-- list of mappings to set on the tree manually
							list = {
								{ key = "n", mode = "n", cb = tree_cb("create") },
								{ key = "u", mode = "n", cb = tree_cb("dir_up") },
								{ key = { "<CR>", "o", "<2-LeftMouse>" }, cb = tree_cb("edit") },
								{ key = { "<2-RightMouse>", "<C-}>" }, cb = tree_cb("cd") },
								{ key = "<C-v>", cb = tree_cb("vsplit") },
								{ key = "<C-x>", cb = tree_cb("split") },
								{ key = "<C-t>", cb = tree_cb("tabnew") },
								{ key = "<", cb = tree_cb("prev_sibling") },
								{ key = ">", cb = tree_cb("next_sibling") },
								{ key = "P", cb = tree_cb("parent_node") },
								{ key = "<BS>", cb = tree_cb("close_node") },
								{ key = "<S-CR>", cb = tree_cb("close_node") },
								{ key = "<Tab>", cb = tree_cb("preview") },
								{ key = "K", cb = tree_cb("first_sibling") },
								{ key = "J", cb = tree_cb("last_sibling") },
								{ key = "I", cb = tree_cb("toggle_ignored") },
								{ key = "H", cb = tree_cb("toggle_dotfiles") },
								{ key = "R", cb = tree_cb("refresh") },
								{ key = "d", cb = tree_cb("remove") },
								{ key = "r", cb = tree_cb("rename") },
								{ key = "<C->", cb = tree_cb("full_rename") },
								{ key = "x", cb = tree_cb("cut") },
								{ key = "c", cb = tree_cb("copy") },
								{ key = "p", cb = tree_cb("paste") },
								{ key = "y", cb = tree_cb("copy_name") },
								{ key = "Y", cb = tree_cb("copy_path") },
								{ key = "gy", cb = tree_cb("copy_absolute_path") },
								{ key = "[c", cb = tree_cb("prev_git_item") },
								{ key = "}c", cb = tree_cb("next_git_item") },
								{ key = "-", cb = tree_cb("dir_up") },
								{ key = "q", cb = tree_cb("close") },
								{ key = "g?", cb = tree_cb("toggle_help") },
							},
						},
					},
				})
			end,
		})

		-- Lua Utils
		use({ "rafcamlet/nvim-luapad" })

		-- Utils
		use({
			"rcarriga/nvim-notify",
			config = function()
				vim.notify = require("notify")
				vim.notify.setup({
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
				})
			end,
		})
		use({ "itchyny/vim-winfix" })
		use({
			"akinsho/toggleterm.nvim",
			config = function()
				require("toggleterm").setup({
					size = 20,
					open_mapping = [[<c-t>]],
					shade_filetypes = {},
					shade_terminals = true,
					direction = "horizontal",
					insert_mappings = true,
					close_on_exit = false,
				})
			end,
		})
		use({
			"moll/vim-bbye",
			config = function()
				vim.api.nvim_set_keymap("n", "<leader>q", "<cmd>:Bdelete<CR>", { noremap = true, silent = true })
				vim.api.nvim_set_keymap("n", "<leader>Q", "<cmd>:Bdelete!<CR>", { noremap = true, silent = true })
			end,
		})
		-- use {'tyru/caw.vim'}
		use({
			"b3nj5m1n/kommentary",
			config = function()
				local config = require("kommentary.config")
				config.use_extended_mappings()
				config.configure_language("default", {
					ignore_whitespace = true,
					use_consistent_indentation = true,
					prefer_single_line_comments = true,
					hook_function = function()
						require("ts_context_commentstring.internal").update_commentstring()
					end,
				})
			end,
		})
		use({
			"JoosepAlviste/nvim-ts-context-commentstring",
			config = function()
				require("nvim-treesitter.configs").setup({
					context_commentstring = { enable = true, enable_autocmd = false },
				})
			end,
		})
		use({ "godlygeek/tabular" })
		use({
			"editorconfig/editorconfig-vim",
			config = function()
				vim.g.EditorConfig_exclude_patterns = { "scp://.*" }
				vim.cmd([[
      au FileType gitcommit let b:EditorConfig_disable = 1
      ]])
			end,
		})
		use({
			"mhartington/formatter.nvim",
			config = function()
				require("formatter_settings")
				vim.api.nvim_set_keymap("n", "<Leader>F", ":Format<cr>", {})
			end,
		})
		use({ "airblade/vim-rooter" })
		use({ "machakann/vim-sandwich" })
		use({ "simeji/winresizer" })
		-- use {'cohama/lexima.vim'}
		use({
			"windwp/nvim-autopairs",
			config = function()
				require("nvim-autopairs").setup({
					disable_filetype = { "TelescopePrompt" },
					ignored_next_char = string.gsub([[ [%w%%%'%[%"%.] ]], "%s+", ""),
					enable_moveright = true,
					enable_afterquote = true, -- add bracket pairs after quote
					enable_check_bracket_line = true, --- check bracket in same line
					check_ts = false,
				})
				if vim.g.lsp_client_type == "neovim" then
					require("nvim-autopairs.completion.cmp").setup({
						map_cr = true, --  map <CR> on insert mode
						map_complete = true, -- it will auto insert `(` after select function or method item
					})
				end
			end,
		})
		use({ "iamcco/markdown-preview.nvim", run = "cd app && yarn install" })
		use({ "npxbr/glow.nvim" })
		use({ "osyo-manga/vim-over" })
		use({ "pechorin/any-jump.vim" })
		use({ "hrsh7th/vim-eft" })
		use({
			"phaazon/hop.nvim",
			config = function()
				vim.api.nvim_set_keymap("n", ",", ":HopWord<cr>", {})
			end,
		})
		use({ "mtdl9/vim-log-highlighting", opt = true })
		use({ "tversteeg/registers.nvim" })
		use({ "bfredl/nvim-miniyank" })
		use({
			"kristijanhusak/orgmode.nvim",
			config = function()
				require("orgmode").setup({
					org_agenda_files = { "~/Dropbox/org/*" },
					org_default_notes_file = "~/Dropbox/org/note.org",
					org_todo_keywords = { "TODO", "DOING", "HOLD", "|", "DONE" },
					org_agenda_templates = {
						t = {
							description = "Task",
							template = "* TODO %?\n  %u",
							target = "~/Dropbox/org/todo.org",
						},
						n = {
							description = "Note",
							template = "* NOTE %?\n  %u",
							target = "~/Dropbox/org/note.org",
						},
						j = {
							description = "Journal",
							template = "\n*** %<%Y-%m-%d> %<%A>\n**** %U\n\n%?",
							target = "~/Dropbox/org/journal.org",
						},
					},
					org_deadline_warning_days = 14,
					org_agenda_span = "week", -- day/week/month/year/number of days
					org_agenda_start_on_weekday = 1,
					org_agenda_start_day = nil, -- start from today + this modifier
					org_priority_highest = "A",
					org_priority_default = "B",
					org_priority_lowest = "C",
					-- org_archive_location = '%s_archive::',
					org_use_tag_inheritance = true,
					-- org_tags_exclude_from_inheritance = {},
					org_hide_leading_stars = false,
					org_hide_emphasis_markers = false,
					org_log_done = "time",
					mappings = {
						disable_all = false,
						global = { org_agenda = "<Leader>oa", org_capture = "<Leader>oc" },
						agenda = {
							org_agenda_later = "f",
							org_agenda_earlier = "b",
							org_agenda_goto_today = ".",
							org_agenda_day_view = "vd",
							org_agenda_week_view = "vw",
							org_agenda_month_view = "vm",
							org_agenda_year_view = "vy",
							org_agenda_quit = "q",
							org_agenda_switch_to = "<CR>",
							org_agenda_goto = { "<TAB>" },
							org_agenda_goto_date = "J",
							org_agenda_redo = "r",
							org_agenda_show_help = "?",
						},
						capture = {
							org_capture_finalize = "<C-c>",
							org_capture_refile = "<Leader>or",
							org_capture_kill = "<Leader>ok",
							org_capture_show_help = "?",
						},
						org = {
							org_refile = "<Leader>or",
							org_increase_date = "<Leader>da",
							org_decrease_date = "<Leader>dx",
							org_change_date = "cid",
							org_todo = "cit",
							org_todo_prev = "ciT",
							org_toggle_checkbox = "<Leader>oT",
							org_open_at_point = "<Leader>oo",
							org_cycle = "<TAB>",
							org_global_cycle = "<S-TAB>",
							org_archive_subtree = "<Leader>o$",
							org_set_tags_command = "<Leader>t",
							org_toggle_archive_tag = "<Leader>oA",
							org_do_promote = "<<",
							org_do_demote = ">>",
							org_promote_subtree = "<s",
							org_demote_subtree = ">s",
							org_meta_return = "<Leader><CR>", -- Add headling, item or row
							org_insert_heading_respect_content = "<Leader>oih", -- Add new headling after current heading block with same level
							org_insert_todo_heading = "<Leader>oiT", -- Add new todo headling right after current heading with same level
							org_insert_todo_heading_respect_content = "<Leader>oit", -- Add new todo headling after current heading block on same level
							org_move_subtree_up = "<Leader>oK",
							org_move_subtree_down = "<Leader>oJ",
							org_show_help = "?",
						},
					},
				})
				vim.api.nvim_set_keymap(
					"n",
					"<Leader>on",
					"<cmd>:e ~/Dropbox/org/note.org<cr>",
					{ silent = true, noremap = true }
				)
				vim.api.nvim_set_keymap(
					"n",
					"<Leader>ot",
					"<cmd>:e ~/Dropbox/org/todo.org<cr>",
					{ silent = true, noremap = true }
				)
				vim.api.nvim_set_keymap(
					"n",
					"<Leader>oj",
					"<cmd>:e ~/Dropbox/org/journal.org<cr>",
					{ silent = true, noremap = true }
				)
			end,
		})
		use({
			"akinsho/org-bullets.nvim",
			requires = { { "kristijanhusak/orgmode.nvim" } },
			config = function()
				require("org-bullets").setup({ symbols = { "◉", "○", "✸", "✿" } })
				vim.cmd([[
          syntax match OrgHeadlineStar1 /^\*\ze\s/me=e-1 conceal cchar=◉ containedin=OrgHeadlineLevel1 contained
          syntax match OrgHeadlineStar2 /^\*\{2}\ze\s/me=e-1 conceal cchar=○ containedin=OrgHeadlineLevel2 contained
          syntax match OrgHeadlineStar3 /^\*\{3}\ze\s/me=e-1 conceal cchar=✸ containedin=OrgHeadlineLevel3 contained
          syntax match OrgHeadlineStar4 /^\*{4}\ze\s/me=e-1 conceal cchar=✿ containedin=OrgHeadlineLevel4 contained
        ]])
			end,
		})

		-- finder

		if vim.g.fuzzy_finder_type == "telescope" then
			if vim.g.lsp_client_type == "coc" then
				use({ "fannheyward/telescope-coc.nvim" })
			end

			use({
				"nvim-telescope/telescope.nvim",
				requires = {
					{ "nvim-lua/plenary.nvim" },
					{ "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
					{ "tkmpypy/telescope-jumps.nvim" },
				},
				config = function()
					local telescope = require("telescope")
					telescope.setup({
						defaults = {
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
							path_display = { "smart" },
							winblend = 10,
							scroll_strategy = "cycle",
							color_devicon = true,
						},
						pickers = {
							buffers = {
								sort_lastused = true,
								theme = "dropdown",
								-- previewer = true,
								mappings = {
									i = { ["<c-d>"] = require("telescope.actions").delete_buffer },
									n = { ["<c-d>"] = require("telescope.actions").delete_buffer },
								},
							},
						},
						extensions = {
							fzf = {
								fuzzy = true, -- false will only do exact matching
								override_generic_sorter = true, -- override the generic sorter
								override_file_sorter = true, -- override the file sorter
								case_mode = "smart_case", -- or "ignore_case" or "respect_case"
								-- the default case_mode is "smart_case"
							},
						},
					})
					telescope.load_extension("fzf")
					telescope.load_extension("jumps")
					if vim.g.lsp_client_type == "coc" then
						telescope.load_extension("coc")
					end
				end,
			})
			use({
				"folke/todo-comments.nvim",
				requires = { "nvim-telescope/telescope.nvim" },
				config = function()
					require("todo-comments").setup({
						-- pattern = "(KEYWORDS)[(.*)]?.*:"
						highlight = {
							before = "", -- "fg" or "bg" or empty
							keyword = "wide", -- "fg", "bg", "wide" or empty. (wide is the same as bg, but will also highlight surrounding characters)
							after = "fg", -- "fg" or "bg" or empty
							pattern = [[.*<(KEYWORDS)\s*:]], -- pattern used for highlightng (vim regex)
							comments_only = true, -- this applies the pattern only inside comments using `commentstring` option
						},
						-- list of named colors where we try to extract the guifg from the
						-- list of hilight groups or use the hex color if hl not found as a fallback
						colors = {
							error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
							warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
							info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
							hint = { "LspDiagnosticsDefaultHint", "#10B981" },
							default = { "Identifier", "#7C3AED" },
						},
						search = {
							command = "rg",
							args = {
								"--color=never",
								"--no-heading",
								"--with-filename",
								"--line-number",
								"--column",
							},
							-- regex that will be used to match keywords.
							-- don't replace the (KEYWORDS) placeholder
							pattern = [[\b(KEYWORDS):]], -- ripgrep regex
							-- pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You'll likely get false positives
						},
					})

					vim.api.nvim_set_keymap(
						"n",
						"<leader>st",
						"<cmd>TodoTelescope<cr>",
						{ silent = true, noremap = true }
					)
				end,
			})
		else
			if vim.g.fuzzy_finder_type == "fzf" then
				use({
					"ibhagwan/fzf-lua",
					requires = {
						"vijaymarupudi/nvim-fzf",
						"kyazdani42/nvim-web-devicons",
					},
					config = function()
						local actions = require("fzf-lua.actions")
						require("fzf-lua").setup({
							winopts = {
								-- split         = "new",           -- open in a split instead?
								win_height = 0.85, -- window height
								win_width = 0.80, -- window width
								win_row = 0.30, -- window row position (0=top, 1=bottom)
								win_col = 0.50, -- window col position (0=left, 1=right)
								-- win_border    = false,           -- window border? or borderchars?
								win_border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
								hl_normal = "Normal", -- window normal color
								hl_border = "FloatBorder", -- window border color
							},
							-- fzf_bin             = 'sk',        -- use skim instead of fzf?
							fzf_layout = "default", -- fzf '--layout='
							fzf_args = "--cycle", -- adv: fzf extra args, empty unless adv
							keymap = {
								builtin = {
									-- neovim `:tmap` mappings for the fzf win
									["<F2>"] = "toggle-fullscreen",
									-- Only valid with the 'builtin' previewer
									["<F3>"] = "toggle-preview-wrap",
									["<F4>"] = "toggle-preview",
									["<S-down>"] = "preview-page-down",
									["<S-up>"] = "preview-page-up",
									["<S-left>"] = "preview-page-reset",
								},
								fzf = {
									-- fzf '--bind=' options
									["ctrl-u"] = "unix-line-discard",
									["ctrl-f"] = "half-page-down",
									["ctrl-b"] = "half-page-up",
									["ctrl-a"] = "beginning-of-line",
									["ctrl-e"] = "end-of-line",
									["alt-a"] = "toggle-all",
									-- Only valid with fzf previewers (bat/cat/git/etc)
									["f3"] = "toggle-preview-wrap",
									["f4"] = "toggle-preview",
									["shift-down"] = "preview-page-down",
									["shift-up"] = "preview-page-up",
								},
							},
							preview_border = "border", -- border|noborder
							preview_wrap = "nowrap", -- wrap|nowrap
							preview_opts = "nohidden", -- hidden|nohidden
							preview_vertical = "down:45%", -- up|down:size
							preview_horizontal = "right:60%", -- right|left:size
							preview_layout = "flex", -- horizontal|vertical|flex
							flip_columns = 120, -- #cols to switch to horizontal on flex
							default_previewer = "bat", -- override the default previewer?
							-- by default uses the builtin previewer
							previewers = {
								cmd = {
									-- custom previewer, will execute:
									-- `<cmd> <args> <filename>`
									cmd = "echo",
									args = "",
								},
								cat = {
									cmd = "cat",
									args = "--number",
								},
								bat = {
									cmd = "bat",
									args = "--style=numbers,changes --color always",
									theme = "Coldark-Dark", -- bat preview theme (bat --list-themes)
									config = nil, -- nil uses $BAT_CONFIG_PATH
								},
								head = {
									cmd = "head",
									args = nil,
								},
								git_diff = {
									cmd = "git diff",
									args = "--color",
								},
								builtin = {
									title = true, -- preview title?
									scrollbar = true, -- scrollbar?
									scrollchar = "█", -- scrollbar character
									syntax = true, -- preview syntax highlight?
									syntax_limit_l = 0, -- syntax limit (lines), 0=nolimit
									syntax_limit_b = 1024 * 1024, -- syntax limit (bytes), 0=nolimit
									expand = false, -- preview max size?
									hl_cursor = "Cursor", -- cursor highlight
									hl_cursorline = "CursorLine", -- cursor line highlight
									hl_range = "IncSearch", -- ranger highlight (not yet in use)
								},
							},
							-- provider setup
							files = {
								-- previewer         = "cat",       -- uncomment to override previewer
								prompt = "Files❯ ",
								cmd = "rg -i --hidden --files -g !.git", -- "find . -type f -printf '%P\n'",
								git_icons = true, -- show git icons?
								file_icons = true, -- show file icons?
								color_icons = true, -- colorize file|git icons
								actions = {
									["default"] = actions.file_edit,
									["ctrl-s"] = actions.file_split,
									["ctrl-v"] = actions.file_vsplit,
									["ctrl-t"] = actions.file_tabedit,
									["ctrl-q"] = actions.file_sel_to_qf,
									["ctrl-y"] = function(selected)
										print(selected[2])
									end,
								},
							},
							git = {
								files = {
									prompt = "GitFiles❯ ",
									cmd = "git ls-files --exclude-standard",
									git_icons = true, -- show git icons?
									file_icons = true, -- show file icons?
									color_icons = true, -- colorize file|git icons
								},
								status = {
									prompt = "GitStatus❯ ",
									cmd = "git status -s",
									previewer = "git_diff",
									file_icons = true,
									git_icons = true,
									color_icons = true,
								},
								commits = {
									prompt = "Commits❯ ",
									cmd = "git log --pretty=oneline --abbrev-commit --color --reflog",
									preview = "git show --pretty='%Cred%H%n%Cblue%an%n%Cgreen%s' --color {1}",
									actions = {
										["default"] = actions.git_checkout,
									},
								},
								bcommits = {
									prompt = "BCommits❯ ",
									cmd = "git log --pretty=oneline --abbrev-commit --color --reflog",
									preview = "git show --pretty='%Cred%H%n%Cblue%an%n%Cgreen%s' --color {1}",
									actions = {
										["default"] = actions.git_buf_edit,
										["ctrl-s"] = actions.git_buf_split,
										["ctrl-v"] = actions.git_buf_vsplit,
										["ctrl-t"] = actions.git_buf_tabedit,
									},
								},
								branches = {
									prompt = "Branches❯ ",
									cmd = "git branch --all --color --reflog",
									preview = "git log --graph --pretty=oneline --abbrev-commit --color {1}",
									actions = {
										["default"] = actions.git_switch,
									},
								},
								icons = {
									-- ["M"] = { icon = "M", color = "yellow" },
									-- ["D"] = { icon = "D", color = "red" },
									-- ["A"] = { icon = "A", color = "green" },
									["?"] = { icon = "?", color = "magenta" },
									["M"] = { icon = "★", color = "red" },
									["D"] = { icon = "✗", color = "red" },
									["A"] = { icon = "+", color = "green" },
								},
							},
							grep = {
								prompt = "Rg❯ ",
								input_prompt = "Grep For❯ ",
								-- cmd               = "rg --vimgrep",
								rg_opts = "--hidden --column --line-number --no-heading "
									.. "--color=always --with-filename --smart-case -g '!{.git,node_modules}/*'",
								git_icons = true, -- show git icons?
								file_icons = true, -- show file icons?
								color_icons = true, -- colorize file|git icons
								actions = {
									["default"] = actions.file_edit,
									["ctrl-s"] = actions.file_split,
									["ctrl-v"] = actions.file_vsplit,
									["ctrl-t"] = actions.file_tabedit,
									["ctrl-q"] = actions.file_sel_to_qf,
									["ctrl-y"] = function(selected)
										print(selected[2])
									end,
								},
							},
							oldfiles = {
								prompt = "History❯ ",
								cwd_only = false,
							},
							buffers = {
								-- previewer      = false,        -- disable the builtin previewer?
								prompt = "Buffers❯ ",
								file_icons = true, -- show file icons?
								color_icons = true, -- colorize file|git icons
								sort_lastused = true, -- sort buffers() by last used
								actions = {
									["default"] = actions.buf_edit,
									["ctrl-s"] = actions.buf_split,
									["ctrl-v"] = actions.buf_vsplit,
									["ctrl-t"] = actions.buf_tabedit,
									["ctrl-d"] = actions.buf_del,
								},
							},
							blines = {
								previewer = "builtin", -- set to 'false' to disable
								prompt = "BLines❯ ",
								actions = {
									["default"] = actions.buf_edit,
									["ctrl-s"] = actions.buf_split,
									["ctrl-v"] = actions.buf_vsplit,
									["ctrl-t"] = actions.buf_tabedit,
								},
							},
							colorschemes = {
								prompt = "Colorschemes❯ ",
								live_preview = true, -- apply the colorscheme on preview?
								actions = {
									["default"] = actions.colorscheme,
									["ctrl-y"] = function(selected)
										print(selected[2])
									end,
								},
								winopts = {
									win_height = 0.55,
									win_width = 0.30,
								},
								post_reset_cb = function()
									-- reset statusline highlights after
									-- a live_preview of the colorscheme
									-- require('feline').reset_highlights()
								end,
							},
							quickfix = {
								-- cwd               = vim.loop.cwd(),
								file_icons = true,
								git_icons = true,
							},
							lsp = {
								prompt = "❯ ",
								-- cwd               = vim.loop.cwd(),
								cwd_only = false, -- LSP/diagnostics for cwd only?
								async_or_timeout = true, -- timeout(ms) or false for blocking calls
								file_icons = true,
								git_icons = false,
								lsp_icons = true,
								severity = "hint",
								icons = {
									["Error"] = { icon = "", color = "red" }, -- error
									["Warning"] = { icon = "", color = "yellow" }, -- warning
									["Information"] = { icon = "", color = "blue" }, -- info
									["Hint"] = { icon = "", color = "magenta" }, -- hint
								},
							},
							-- placeholders for additional user customizations
							loclist = {},
							helptags = {},
							manpages = {},
							-- optional override of file extension icon colors
							-- available colors (terminal):
							--    clear, bold, black, red, green, yellow
							--    blue, magenta, cyan, grey, dark_grey, white
							-- padding can help kitty term users with
							-- double-width icon rendering
							file_icon_padding = "",
							file_icon_colors = {
								["lua"] = "blue",
							},
						})
						-- Buffer
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sb",
							"<cmd>lua require('fzf-lua').buffers()<CR>",
							{ noremap = true, silent = true }
						)
						-- File
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sfg",
							"<cmd>lua require('fzf-lua').git_files()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sff",
							"<cmd>lua require('fzf-lua').files()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sfr",
							"<cmd>lua require('fzf-lua').files_resume()<CR>",
							{ noremap = true, silent = true }
						)
						-- Git
						vim.api.nvim_set_keymap(
							"n",
							"<leader>svc",
							"<cmd>lua require('fzf-lua').git_bcommits()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>svC",
							"<cmd>lua require('fzf-lua').git_commits()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>svs",
							"<cmd>lua require('fzf-lua').git_status()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>svb",
							"<cmd>lua require('fzf-lua').git_branches()<CR>",
							{ noremap = true, silent = true }
						)
						-- Grep
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sgg",
							"<cmd>lua require('fzf-lua').live_grep()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sgr",
							"<cmd>lua require('fzf-lua').live_grep_resume()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sgw",
							"<cmd>lua require('fzf-lua').grep_cword()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sgl",
							"<cmd>lua require('fzf-lua').blines()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sgc",
							"<cmd>lua require('fzf-lua').grep_curbuf()<CR>",
							{ noremap = true, silent = true }
						)
						-- Misc
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sc",
							"<cmd>lua require('fzf-lua').command_history()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sr",
							"<cmd>lua require('fzf-lua').oldfiles()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sl",
							"<cmd>lua require('fzf-lua').loclist()<CR>",
							{ noremap = true, silent = true }
						)
						vim.api.nvim_set_keymap(
							"n",
							"<leader>sq",
							"<cmd>lua require('fzf-lua').quickfix()<CR>",
							{ noremap = true, silent = true }
						)
						if vim.g.lsp_client_type == "neovim" then
							vim.api.nvim_set_keymap(
								"n",
								"gr",
								"<cmd>lua require('fzf-lua').lsp_references()<CR>",
								{ noremap = true, silent = true }
							)
						end
					end,
				})
			end
		end

		-- Git
		use({
			"lewis6991/gitsigns.nvim",
			requires = { "nvim-lua/plenary.nvim" },
			config = function()
				require("gitsigns").setup()
			end,
		})
		use({
			"lambdalisue/gina.vim",
			disable = true,
			config = function()
				vim.fn["gina#custom#mapping#nmap"](
					"status",
					"dd",
					":<C-u>Gina diff --opener=vsplit<CR>",
					{ noremap = 1, silent = 1 }
				)
				vim.fn["gina#custom#mapping#nmap"](
					"status",
					"dp",
					":<C-u>Gina diff --opener=preview<CR>",
					{ noremap = 1, silent = 1 }
				)
				vim.cmd([[
        nnoremap <leader>gs :<C-u>Gina status --opener=split<CR>
        nnoremap <leader>gc :<C-u>Gina commit --opener=vsplit<CR>
        nnoremap <leader>gD :<C-u>Gina compare --opener=tabedit<CR>
        nnoremap <leader>gd :<C-u>Gina diff --opener=tabedit<CR>
        nnoremap <leader>gl :<C-u>Gina log --graph --opener=tabedit<CR>
        nnoremap <leader>gb :<C-u>Gina blame --opener=vsplit<CR>
        vnoremap <leader>gln :<C-u>:'<,'>Gina browse --exact --yank :<CR>
        nnoremap <leader>gln :<C-u>:Gina browse --exact --yank :<CR>
        nnoremap <leader>gm :<C-u>:Gina chaperon<CR>
        nnoremap <leader>gp :<C-u>Gina push<CR>
      ]])
			end,
		})
		use({
			"sindrets/diffview.nvim",
			config = function()
				local cb = require("diffview.config").diffview_callback
				require("diffview").setup({
					diff_binaries = false, -- Show diffs for binaries
					enhanced_diff_hl = true, -- See ':h diffview-config-enhanced_diff_hl'
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
						position = "left", -- One of 'left', 'right', 'top', 'bottom'
						width = 35, -- Only applies when position is 'left' or 'right'
						height = 10, -- Only applies when position is 'top' or 'bottom'
						listing_style = "tree", -- One of 'list' or 'tree'
						tree_options = { -- Only applies when listing_style is 'tree'
							flatten_dirs = true,
							folder_statuses = "always", -- One of 'never', 'only_folded' or 'always'.
						},
					},
					file_history_panel = {
						position = "bottom",
						width = 35,
						height = 16,
						log_options = {
							max_count = 256, -- Limit the number of commits
							follow = false, -- Follow renames (only for single file)
							all = false, -- Include all refs under 'refs/' including HEAD
							merges = false, -- List only merge commits
							no_merges = false, -- List no merge commits
							reverse = false, -- List commits in reverse order
						},
					},
					key_bindings = {
						disable_defaults = false, -- Disable the default key bindings
						-- The `view` bindings are active in the diff buffers, only when the current
						-- tabpage is a Diffview.
						view = {
							["<tab>"] = cb("select_next_entry"), -- Open the diff for the next file
							["<s-tab>"] = cb("select_prev_entry"), -- Open the diff for the previous file
							["gf"] = cb("goto_file"), -- Open the file in a new split in previous tabpage
							["<C-w><C-f>"] = cb("goto_file_split"), -- Open the file in a new split
							["<C-w>gf"] = cb("goto_file_tab"), -- Open the file in a new tabpage
							["<leader>e"] = cb("focus_files"), -- Bring focus to the files panel
							["<leader>b"] = cb("toggle_files"), -- Toggle the files panelos.time
							["q"] = "<CMD>DiffviewClose<CR>",
						},
						file_panel = {
							["j"] = cb("next_entry"), -- Bring the cursor to the next file entry
							["<down>"] = cb("next_entry"),
							["k"] = cb("prev_entry"), -- Bring the cursor to the previous file entry.
							["<up>"] = cb("prev_entry"),
							["<cr>"] = cb("select_entry"), -- Open the diff for the selected entry.
							["o"] = cb("select_entry"),
							["<2-LeftMouse>"] = cb("select_entry"),
							["-"] = cb("toggle_stage_entry"), -- Stage / unstage the selected entry.
							["S"] = cb("stage_all"), -- Stage all entries.
							["U"] = cb("unstage_all"), -- Unstage all entries.
							["X"] = cb("restore_entry"), -- Restore entry to the state on the left side.
							["R"] = cb("refresh_files"), -- Update stats and entries in the file list.
							["<tab>"] = cb("select_next_entry"),
							["<s-tab>"] = cb("select_prev_entry"),
							["gf"] = cb("goto_file"),
							["<C-w><C-f>"] = cb("goto_file_split"),
							["<C-w>gf"] = cb("goto_file_tab"),
							["i"] = cb("listing_style"), -- Toggle between 'list' and 'tree' views
							["f"] = cb("toggle_flatten_dirs"), -- Flatten empty subdirectories in tree listing style.
							["<leader>e"] = cb("focus_files"),
							["<leader>b"] = cb("toggle_files"),
							["q"] = "<CMD>DiffviewClose<CR>",
						},
						file_history_panel = {
							["g!"] = cb("options"), -- Open the option panel
							["<C-d>"] = cb("open_in_diffview"), -- Open the entry under the cursor in a diffview
							["zR"] = cb("open_all_folds"),
							["zM"] = cb("close_all_folds"),
							["j"] = cb("next_entry"),
							["<down>"] = cb("next_entry"),
							["k"] = cb("prev_entry"),
							["<up>"] = cb("prev_entry"),
							["<cr>"] = cb("select_entry"),
							["o"] = cb("select_entry"),
							["<2-LeftMouse>"] = cb("select_entry"),
							["<tab>"] = cb("select_next_entry"),
							["<s-tab>"] = cb("select_prev_entry"),
							["gf"] = cb("goto_file"),
							["<C-w><C-f>"] = cb("goto_file_split"),
							["<C-w>gf"] = cb("goto_file_tab"),
							["<leader>e"] = cb("focus_files"),
							["<leader>b"] = cb("toggle_files"),
							["q"] = "<CMD>DiffviewClose<CR>",
						},
						option_panel = {
							["<tab>"] = cb("select"),
							["q"] = cb("close"),
						},
					},
				})
				vim.api.nvim_set_keymap("n", "<leader>gdd", "<cmd>DiffviewOpen<cr>", { silent = true, noremap = true })
				vim.api.nvim_set_keymap(
					"n",
					"<leader>gdc",
					"<cmd>DiffviewFileHistory .<cr>",
					{ silent = true, noremap = true }
				)
			end,
		})
		use({
			"TimUntersberger/neogit",
			requires = { { "nvim-lua/plenary.nvim" }, { "sindrets/diffview.nvim" } },
			config = function()
				local neogit = require("neogit")
				neogit.setup({
					disable_signs = true,
					disable_context_highlighting = false,
					disable_commit_confirmation = false,
					auto_refresh = true,
					disable_builtin_notifications = false,
					commit_popup = {
						kind = "split",
					},
					-- customize displayed signs
					signs = {
						-- { CLOSED, OPENED }
						section = { ">", "v" },
						item = { ">", "v" },
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

				vim.api.nvim_set_keymap(
					"n",
					"<leader>gs",
					"<cmd>Neogit kind=split<cr>",
					{ silent = true, noremap = true }
				)
			end,
		})
		use({ "rhysd/git-messenger.vim" })
		use({ "APZelos/blamer.nvim" })
		use({
			"pwntester/octo.nvim",
			config = function()
				require("octo").setup({
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
				})
				--[[ vim.api.nvim_set_keymap("n", "<leader>gil",
                          "<cmd>Octo issue list<cr>",
                          {silent = true, noremap = true})
      vim.api.nvim_set_keymap("n", "<leader>gpl",
                          "<cmd>Octo pr list<cr>",
                          {silent = true, noremap = true}) ]]
			end,
		})

		-- Snippet
		use({ "mattn/vim-sonictemplate" })
		if vim.g.lsp_client_type == "neovim" then
			-- use neovim built-in
			use({ "neovim/nvim-lspconfig" })
			use({ "kabouzeid/nvim-lspinstall", disable = true })
			use({
				"williamboman/nvim-lsp-installer",
				requires = { { "neovim/nvim-lspconfig" } },
			})
			use({ "nvim-lua/lsp-status.nvim", disable = true })
			use({ "tjdevries/lsp_extensions.nvim", disable = true })
			use({
				"mfussenegger/nvim-lint",
				disable = true,
				config = function()
					local lint = require("lint")
					lint.linters.eslint.cmd = "./node_modules/.bin/eslint"
					lint.linters_by_ft = {
						markdown = { "vale" },
						python = { "flake8" },
						typescript = { "eslint" },
						typescriptreact = { "eslint" },
						javascript = { "eslint" },
						javascriptreact = { "eslint" },
						go = { "golangcilint" },
					}

					vim.cmd([[
            augroup linter
              autocmd!
              autocmd BufEnter,BufWritePost * lua require('lint').try_lint()
            augroup end
          ]])
				end,
			})
			use({
				"jose-elias-alvarez/null-ls.nvim",
				disable = true,
				requires = { { "neovim/nvim-lspconfig" }, { "nvim-lua/plenary.nvim" } },
				config = function()
					local null_ls = require("null-ls")
					require("null-ls").config({
						sources = {
							null_ls.builtins.diagnostics.markdownlint,
							null_ls.builtins.diagnostics.eslint.with({ command = "./node_modules/.bin/eslint" }),
							null_ls.builtins.diagnostics.flake8,
						},
						diagnostics_format = "#{m}",
						debounce = 250,
						default_timeout = 5000,
						debug = true,
					})

					require("lspconfig")["null-ls"].setup({})
				end,
			})
			use({ "hrsh7th/vim-vsnip" })
			use({ "hrsh7th/vim-vsnip-integ", requires = { { "hrsh7th/vim-vsnip" } } })
			use({ "hrsh7th/cmp-vsnip", requires = { "hrsh7th/nvim-cmp" } })
			use({ "hrsh7th/cmp-buffer", requires = { "hrsh7th/nvim-cmp" } })
			use({ "hrsh7th/cmp-path", requires = { "hrsh7th/nvim-cmp" } })
			use({ "hrsh7th/cmp-nvim-lua", requires = { "hrsh7th/nvim-cmp" } })
			use({
				"hrsh7th/cmp-nvim-lsp",
				requires = { "hrsh7th/nvim-cmp" },
				config = function()
					require("cmp_nvim_lsp").setup({})
				end,
			})
			use({
				"hrsh7th/nvim-cmp",
				requires = {
					{ "onsails/lspkind-nvim" },
					{ "hrsh7th/vim-vsnip" },
					{ "hrsh7th/cmp-vsnip" },
					{ "hrsh7th/cmp-buffer" },
					{ "hrsh7th/cmp-path" },
					{ "hrsh7th/cmp-nvim-lsp" },
					{ "hrsh7th/cmp-nvim-lua" },
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
						completion = {
							autocomplete = { types.cmp.TriggerEvent.TextChanged },
							completeopt = "menu,menuone,noselect",
							keyword_pattern = [[\%(-\?\d\+\%(\.\d\+\)\?\|\h\w*\%(-\w*\)*\)]],
							keyword_length = 1,
						},
						sorting = {
							priority_weight = 2,
							comparators = {
								function(e1, e2)
									local diff
									diff = compare.offset(e1, e2)
									if diff ~= nil then
										return diff
									end
									diff = compare.exact(e1, e2)
									if diff ~= nil then
										return diff
									end
									diff = compare.score(e1, e2)
									if diff ~= nil then
										return diff
									end
									diff = compare.kind(e1, e2)
									if diff ~= nil then
										return diff
									end
									diff = compare.sort_text(e1, e2)
									if diff ~= nil then
										return diff
									end
									diff = compare.length(e1, e2)
									if diff ~= nil then
										return diff
									end
									return compare.order(e1, e2)
								end,
							},
						},
						-- You should change this example to your chosen snippet engine.
						snippet = {
							expand = function(args)
								-- You must install `vim-vsnip` if you set up as same as the following.
								vim.fn["vsnip#anonymous"](args.body)
							end,
						},

						-- You must set mapping.
						mapping = {
							["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
							["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
							["<C-d>"] = cmp.mapping.scroll_docs(-4),
							["<C-b>"] = cmp.mapping.scroll_docs(4),
							["<C-Space>"] = cmp.mapping.complete(),
							["<C-e>"] = cmp.mapping.close(),
							["<C-c>"] = function()
								local key = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
								vim.api.nvim_feedkeys(key, "i", false)
							end,
							["<CR>"] = cmp.mapping.confirm({ select = true }),
						},

						-- You should specify your *installed* sources.
						sources = {
							{ name = "nvim_lsp" },
							{ name = "vsnip" },
							{ name = "path" },
							{ name = "orgmode" },
							{ name = "nvim_lua" },
							{
								name = "buffer",
								opts = {
									-- get_bufnrs = function()
									-- 	return vim.api.nvim_list_bufs()
									-- end,
									get_bufnrs = function()
										local bufs = {}
										for _, win in ipairs(vim.api.nvim_list_wins()) do
											bufs[vim.api.nvim_win_get_buf(win)] = true
										end
										return vim.tbl_keys(bufs)
									end,
								},
							},
						},
						formatting = {
							format = lspkind.cmp_format(),
						},
						experimental = {
							native_menu = false,
							ghost_text = true,
						},
					})
				end,
			})
			use({
				"onsails/lspkind-nvim",
				config = function()
					require("lspkind").init({ with_text = false })
				end,
			})
			use({
				"glepnir/lspsaga.nvim",
				disable = true,
				require = { { "neovim/nvim-lspconfig" } },
				config = function()
					local saga = require("lspsaga")
					saga.init_lsp_saga({
						-- add your config value here
						-- default value
						use_saga_diagnostic_sign = false,
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
					})
				end,
			})
			use({
				"folke/lsp-trouble.nvim",
				requires = "kyazdani42/nvim-web-devicons",
				config = function()
					require("trouble").setup({
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
					})

					vim.api.nvim_set_keymap(
						"n",
						"<leader>sd",
						"<cmd>LspTroubleDocumentToggle<cr>",
						{ silent = true, noremap = true }
					)
					vim.api.nvim_set_keymap(
						"n",
						"<leader>sD",
						"<cmd>LspTroubleWorkspaceToggle<cr>",
						{ silent = true, noremap = true }
					)
				end,
			})

			use({
				"akinsho/flutter-tools.nvim",
				requires = { "neovim/nvim-lspconfig" },
				ft = { "dart" },
			})
		elseif vim.g.lsp_client_type == "coc" then
			use({
				"neoclide/coc.nvim",
				branch = "master",
				run = "yarn install --frozen-lockfile",
				requires = { "rafcamlet/coc-nvim-lua" },
			})
		end

		use({
			"tkmpypy/chowcho.nvim",
			config = function()
				require("chowcho").setup({ border_style = "rounded", icon_enabled = true })
			end,
		})
	end,
	config = {
		-- Move to lua dir so impatient.nvim can cache it
		compile_path = packer_compiled_path,
		display = { open_fn = packer_util.float },
	},
})
