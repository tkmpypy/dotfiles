-- This file can be loaded by calling `lua require('plugins')` from your init.vim
-- Only required if you have packer in your `opt` pack
vim.cmd [[packadd packer.nvim]]
-- Only if your version of Neovim doesn't have https://github.com/neovim/neovim/pull/12632 merged
-- vim._update_package_paths()

local packer = require('packer')
local use = packer.use
local util = require('packer.util')

packer.startup {
  function()
    -- Packer can manage itself as an optional plugin
    use {'wbthomason/packer.nvim', opt = true}

    if (vim.g.use_treesitter) then
      use {'nvim-treesitter/nvim-treesitter'}
    end

    -- ColorScheme
    use {'altercation/vim-colors-solarized', opt = true}
    use {'Rigellute/rigel', opt = true}
    use {'w0ng/vim-hybrid', opt = true}
    use {'flrnprz/plastic.vim', opt = true}
    use {'KeitaNakamura/neodark.vim', opt = true}
    use {'joshdick/onedark.vim', opt = true}
    use {'sonph/onehalf', opt = true}
    use {'cocopon/iceberg.vim', opt = true}
    use {'dracula/vim', opt = true}
    use {'morhetz/gruvbox', opt = true}
    use {'kaicataldo/material.vim', opt = true}
    use {'edkolev/tmuxline.vim', opt = true}
    use {'sainnhe/edge', opt = true}
    use {'sainnhe/gruvbox-material', opt = true}
    use {'arcticicestudio/nord-vim', opt = true}
    use {'whatyouhide/vim-gotham', opt = true}
    use {'yuttie/hydrangea-vim', opt = true}
    use {'NLKNguyen/papercolor-theme', opt = true}
    use {'junegunn/seoul256.vim', opt = true}
    use {'ghifarit53/tokyonight-vim', opt = true}
    use {'drewtempelmeyer/palenight.vim', opt = true}
    use {'franbach/miramare', opt = true}

    -- supported treesitter colorscheme
    use {'mhartington/oceanic-next'}

    -- Languages
    use {'plasticboy/vim-markdown', ft = {'markdown'}}
    use {'thosakwe/vim-flutter', ft = {'dart'}}
    use {'sheerun/vim-polyglot'}
    use {'euclidianAce/BetterLua.vim', ft = {'lua'}}

    -- runner
    use {'metakirby5/codi.vim'}
    use {'vim-test/vim-test'}
    use {'thinca/vim-quickrun'}

    -- UI
    use {'luochen1990/rainbow'}
    use {'tjdevries/cyclist.vim'}
    use {'norcalli/nvim-colorizer.lua'}
    use {'google/vim-searchindex'}
    use {'kyazdani42/nvim-web-devicons'}
    use {
      'glepnir/galaxyline.nvim',
      branch = 'main',
      config = function() return require('statusline') end,
      requires = {'kyazdani42/nvim-web-devicons', opt = true}
    }
    -- use {'akinsho/nvim-bufferline.lua'}
    use {'romgrk/barbar.nvim', requires = {{'romgrk/lib.kom'}}}
    -- use {'glepnir/indent-guides.nvim'}
    use {'mhinz/vim-startify'}
    use {'liuchengxu/vista.vim'}

    -- tree
    use {'kyazdani42/nvim-tree.lua'}

    -- Lua Utils
    use {'tjdevries/nlua.nvim'}
    use {'rafcamlet/nvim-luapad'}

    -- Utils
    use {'akinsho/nvim-toggleterm.lua'}
    use {'moll/vim-bbye'}
    use {'tyru/caw.vim'}
    use {'godlygeek/tabular'}
    use {'lukas-reineke/format.nvim'}
    use {'airblade/vim-rooter'}
    use {'machakann/vim-sandwich'}
    use {'simeji/winresizer'}
    use {'cohama/lexima.vim'}
    use {'iamcco/markdown-preview.nvim', run = 'cd app && yarn install'}
    use {'glidenote/memolist.vim'}
    use {'mbbill/undotree'}
    use {'osyo-manga/vim-over'}
    use {'tyru/operator-camelize.vim', requires = {{'kana/vim-operator-user'}}}
    use {'pechorin/any-jump.vim'}
    use {'hrsh7th/vim-eft'}
    use {'mtdl9/vim-log-highlighting'}

    -- finder
    use {
      'nvim-telescope/telescope.nvim',
      requires = {{'nvim-lua/plenary.nvim'}, {'nvim-lua/popup.nvim'}}
    }

    -- Git
    use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'}}
    use {'lambdalisue/gina.vim'}
    use {'rhysd/git-messenger.vim'}
    use {'gotchane/vim-git-commit-prefix'}
    use {'APZelos/blamer.nvim'}

    -- service
    use {'wakatime/vim-wakatime'}

    -- Fix
    -- see https://github.com/neovim/neovim/issues/12587
    use {'antoinemadec/FixCursorHold.nvim'}

    if (vim.g.use_builtin_lsp) then
      -- use neovim built-in
      use {'neovim/nvim-lspconfig'}
      use {
        'hrsh7th/nvim-compe',
        requires = {
          {'hrsh7th/vim-vsnip-integ', opt = true},
          {'hrsh7th/vim-vsnip', opt = true}
        }
      }
      -- use {
      --   'nvim-lua/completion-nvim',
      --   requires = {
      --     {'steelsojka/completion-buffers', opt = true},
      --     {'hrsh7th/vim-vsnip-integ', opt = true},
      --     {'hrsh7th/vim-vsnip', opt = true},
      --   }
      -- }
      use {'nvim-lua/lsp-status.nvim'}
      use {'RishabhRD/nvim-lsputils', requires = {{'RishabhRD/popfix'}}}
      use {'tjdevries/lsp_extensions.nvim'}
    else
      use {'neoclide/coc.nvim', run = 'yarn install --frozen-lockfile'}
    end

    use {'~/private/pika.nvim'}
    use {'~/private/chowcho.nvim'}
  end,
  config = {
    display = {
      open_fn = util.float
    }
  }
}

vim.cmd [[autocmd BufWritePost plugins.lua PackerCompile]]
