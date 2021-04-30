local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim ' ..
              install_path)
end

execute 'packadd packer.nvim'
local packer = require('packer')
local use = packer.use
local util = require('packer.util')

packer.init({ensure_dependencies = true, display = {open_fn = util.float}})

-- Packer can manage itself as an optional plugin
use {'wbthomason/packer.nvim', opt = true}

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
use {'glepnir/zephyr-nvim', requires = {{'nvim-treesitter/nvim-treesitter'}}}
use {'savq/melange', opt = true}
use {'folke/tokyonight.nvim'}
use {'Th3Whit3Wolf/space-nvim', config = function ()
  vim.g.space_nvim_transparent_bg = true
end}

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

-- runner
use {'metakirby5/codi.vim'}
use {'vim-test/vim-test'}
use {'thinca/vim-quickrun'}

-- UI
use {'p00f/nvim-ts-rainbow', config = function ()
  require'nvim-treesitter.configs'.setup {
  rainbow = {
    enable = true,
    extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
  }
}
end}
use {
  'sunjon/shade.nvim',
  disable=true,
  config = function ()
    require'shade'.setup({
      overlay_opacity = 50,
      opacity_step = 1,
      keys = {
        brightness_up    = '<C-Up>',
        brightness_down  = '<C-Down>',
      }
    })
end}

use {'tjdevries/cyclist.vim'}
use {'norcalli/nvim-colorizer.lua'}
use {'kyazdani42/nvim-web-devicons'}
use {
  'glepnir/galaxyline.nvim',
  branch = 'main',
  config = function() return require('statusline') end,
  requires = {'kyazdani42/nvim-web-devicons'}
}
use {'akinsho/nvim-bufferline.lua',
  requires = {'kyazdani42/nvim-web-devicons'},
  config = function ()
    require'bufferline'.setup{
      options = {
        view = "multiwindow", -- "multiwindow" | "default"
        numbers = "ordinal", -- "none" | "ordinal" | "buffer_id"
        number_style = "superscript",
        mappings = true,
        buffer_close_icon= '',
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
use {'kyazdani42/nvim-tree.lua'}

-- Lua Utils
use {'rafcamlet/nvim-luapad'}

-- Utils
use {'itchyny/vim-winfix'}
use {'akinsho/nvim-toggleterm.lua', config = function ()
  require"toggleterm".setup{
    size = 20,
    open_mapping = [[<c-\>]],
    shade_filetypes = {},
    shade_terminals = true,
    direction = 'horizontal'
  }
end}
use {'moll/vim-bbye'}
-- use {'tyru/caw.vim'}
use {
  'b3nj5m1n/kommentary',
  config = function()
    local config = require('kommentary.config')
    config.use_extended_mappings()
    config.configure_language("default", {ignore_whitespace = true})
  end
}
use {'godlygeek/tabular'}
use {'lukas-reineke/format.nvim'}
use {'airblade/vim-rooter'}
use {'machakann/vim-sandwich'}
use {'simeji/winresizer'}
use {'cohama/lexima.vim'}
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
  config = function() vim.api.nvim_set_keymap('n', ',', ":HopWord<cr>", {}) end
}
use {'mtdl9/vim-log-highlighting', opt = true}
use {'tversteeg/registers.nvim'}
-- use {'gelguy/wilder.nvim', run = ':UpdateRemotePlugins'}
use {'bfredl/nvim-miniyank'}

-- finder
use {
  'nvim-telescope/telescope.nvim',
  requires = {
    {'nvim-lua/plenary.nvim'}, {'nvim-lua/popup.nvim'},
    {'tkmpypy/telescope-jumps.nvim'}
  },
  config = function ()
    local telescope = require('telescope')
    telescope.load_extension('jumps')
    telescope.setup{
      defaults = {
        vimgrep_arguments = {
          'rg',
          '--color=never',
          '--no-heading',
          '--with-filename',
          '--line-number',
          '--column',
          '--smart-case',
          '--hidden'
        },
        shortlen_path = true,
        winblend = 10,
        scroll_strategy = 'cycle',
        color_devicon = true,
      }
    }
  end
}

-- Git
use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'}}
use {'lambdalisue/gina.vim'}
use {'rhysd/git-messenger.vim'}
use {'APZelos/blamer.nvim'}

-- Snippet
-- use {'hrsh7th/vim-vsnip', rerequires = {{'hrsh7th/vim-vsnip-integ'}}}

if (vim.g.lsp_client_type == 'neovim') then
  -- use neovim built-in
  use {'neovim/nvim-lspconfig'}
  use {'kabouzeid/nvim-lspinstall'}
  use {'hrsh7th/nvim-compe', requires = {{'hrsh7th/vim-vsnip'}}}
  use {
    'onsails/lspkind-nvim',
    config = function()
      require('lspkind').init({
        with_text = true,
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
        },
      })
    end
  }

  use {'nvim-lua/lsp-status.nvim'}
  use {'tjdevries/lsp_extensions.nvim'}
  use {
    'glepnir/lspsaga.nvim',
    -- disable = true,
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

      vim.api.nvim_set_keymap("n", "<leader>sd", "<cmd>LspTroubleDocumentToggle<cr>",
        {silent = true, noremap = true}
      )
      vim.api.nvim_set_keymap("n", "<leader>sD", "<cmd>LspTroubleWorkspaceToggle<cr>",
        {silent = true, noremap = true}
      )
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
  'tkmpypy/scrapaper.nvim',
  config = function()
    require('scrapaper').setup {filepath = '~/Dropbox/scrap.md', h_level = 2}
  end
}
