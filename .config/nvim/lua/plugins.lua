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
use {'joshdick/onedark.vim', opt = true}
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
use {'ghifarit53/tokyonight-vim', opt = true}
use {'drewtempelmeyer/palenight.vim'}
use {'franbach/miramare', opt = true}
use {'embark-theme/vim', opt = true, as = 'embark'}

-- supported treesitter colorscheme
use {'mhartington/oceanic-next', opt = true}
use {'sainnhe/edge'}
use {'glepnir/zephyr-nvim', requires = {{'nvim-treesitter/nvim-treesitter'}}}
use {'savq/melange', opt = true}

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
use {'luochen1990/rainbow'}
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
use {'npxbr/glow.nvim', run = ':GlowInstall'}
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
use {'gelguy/wilder.nvim'}

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

-- service
use {'wakatime/vim-wakatime'}

-- Snippet
-- use {'hrsh7th/vim-vsnip', rerequires = {{'hrsh7th/vim-vsnip-integ'}}}

if (vim.g.lsp_client_type == 'neovim') then
  -- use neovim built-in
  use {'neovim/nvim-lspconfig'}
  -- TODO
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
    require = {{'nvim-lua/lsp-status.nvim'}},
    config = function()
      local saga = require('lspsaga')
      saga.init_lsp_saga {
        -- add your config value here
        -- default value
        -- use_saga_diagnostic_handler = false // disable the lspsaga diagnostic handler
        use_saga_diagnostic_sign = false,
        -- error_sign = '',
        -- warn_sign = '',
        -- hint_sign = '',
        -- infor_sign = '',
        -- code_action_icon = ' ',
        -- finder_definition_icon = '  ',
        -- finder_reference_icon = '  ',
        -- definition_preview_icon = '  '
        -- 1: thin border | 2: rounded border | 3: thick border
        border_style = 2
        -- max_hover_width = 0 (automatically adjust to the width of current symbol)
      }
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
-- use {
--   '~/private/complua.nvim',
--   config = function()
--     require('complua').setup {
--       enable = true,
--       debug = true,
--       wait_time = 20,
--       mapping = {
--         confirm = '<C-y>'
--       },
--       trigger = {
--         char = {'.', ':'},
--         min_length = 1
--       },
--       match = {
--         ignore_case = true,
--         smart_case = true
--       },
--       sources = {
--         buffer = {
--           priority = 5,
--           label = '[BUFFER]',
--           -- filetypes = {'go'},
--           additional_params = {
--             only_current = false
--           }
--         },
--         filepath = false,
--         nvim_lsp = {
--           priority = 15,
--           filetypes = {},
--           label = '[LSP]',
--         }
--       }
--     }
--   end,
--   requires = {{'hrsh7th/vim-vsnip'}, {'hrsh7th/vim-vsnip-integ'}}
-- }
