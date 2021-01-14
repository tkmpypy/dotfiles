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
use {'dracula/vim', opt = true}
use {'morhetz/gruvbox', opt = true}
use {'kaicataldo/material.vim', opt = true}
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
use {'sainnhe/edge'}
use {'glepnir/zephyr-nvim', requires = {{'nvim-treesitter/nvim-treesitter'}}}
use {'Th3Whit3Wolf/onebuddy', requires = {{'tjdevries/colorbuddy.vim'}}}

-- Languages
use {'plasticboy/vim-markdown', ft = {'markdown'}}
use {'sheerun/vim-polyglot'}
use {'euclidianAce/BetterLua.vim', ft = {'lua'}}

-- runner
use {'metakirby5/codi.vim'}
use {'vim-test/vim-test'}
use {'thinca/vim-quickrun'}

-- UI
use {'kevinhwang91/nvim-hlslens'}
use {'luochen1990/rainbow'}
use {'tjdevries/cyclist.vim'}
use {'norcalli/nvim-colorizer.lua'}
use {'google/vim-searchindex'}
use {'kyazdani42/nvim-web-devicons'}
use {
  'glepnir/galaxyline.nvim',
  branch = 'main',
  config = function() return require('statusline') end,
  requires = {'kyazdani42/nvim-web-devicons'}
}
use {'akinsho/nvim-bufferline.lua', requires = {'kyazdani42/nvim-web-devicons'}}
-- use {'romgrk/barbar.nvim', requires = {{'romgrk/lib.kom'}}}
-- use {'glepnir/indent-guides.nvim'}
use {'mhinz/vim-startify'}
use {'liuchengxu/vista.vim'}

-- tree
use {'kyazdani42/nvim-tree.lua'}

-- Lua Utils
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
use {'npxbr/glow.nvim', run = ':GlowInstall'}
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

if (vim.g.lsp_client_type == 'neovim') then
  -- use neovim built-in
  use {'neovim/nvim-lspconfig'}
  -- use {
  --   'hrsh7th/nvim-compe',
  --   requires = {{'hrsh7th/vim-vsnip-integ'}, {'hrsh7th/vim-vsnip'}}
  -- }
  use {
    'nvim-lua/completion-nvim',
    requires = {{'hrsh7th/vim-vsnip-integ'}, {'hrsh7th/vim-vsnip'}}
  }
  use {'steelsojka/completion-buffers', after = 'completion-nvim'}

  use {'nvim-lua/lsp-status.nvim'}
  use {'RishabhRD/nvim-lsputils', requires = {{'RishabhRD/popfix'}}}
  use {'tjdevries/lsp_extensions.nvim'}

  use {"akinsho/flutter-tools.nvim", requires = {"neovim/nvim-lspconfig"}, ft = {'dart'}}
elseif (vim.g.lsp_client_type == 'coc') then
  use {'neoclide/coc.nvim', run = 'yarn install --frozen-lockfile', requires = {'rafcamlet/coc-nvim-lua'}}
end

-- use {'~/private/pika.nvim'}
-- use {'~/private/chowcho.nvim'}
use {'~/private/fb.nvim'}
use {'~/private/complua.nvim'}

packer.compile('~/.cache/nvim/plugin/packer_load.vim')
