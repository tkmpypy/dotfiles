local function isModuleAvailable(name)
  if package.loaded[name] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
      local loader = searcher(name)
      if type(loader) == 'function' then
        package.preload[name] = loader
        return true
      end
    end
    return false
  end
end

if isModuleAvailable('nvim-treesitter.configs') then
    require'nvim-treesitter.configs'.setup {
        highlight = {
            enable = true,
        },
        incremental_selection = {
            enable = true,
        },
        refactor = {
          highlight_defintions = {
            enable = true
          },
          smart_rename = {
            enable = false,
          },
          navigation = {
            enable = false,
          }
        },
        ensure_installed = 'all'
    }
end

local lsp_status = require('lsp-status')
lsp_status.register_progress()

local nvim_lsp = require('nvim_lsp')
nvim_lsp.jsonls.setup({
  on_attach = lsp_status.on_attach,
  capabilities = lsp_status.capabilities
})

nvim_lsp.pyls_ms.setup({
  callbacks = lsp_status.extensions.pyls_ms.setup(),
  settings = { python = { workspaceSymbols = { enabled = true }}},
  on_attach = lsp_status.on_attach,
  capabilities = lsp_status.capabilities
})

nvim_lsp.vimls.setup({
  on_attach = lsp_status.on_attach,
  capabilities = lsp_status.capabilities
})
nvim_lsp.rust_analyzer.setup({
  on_attach = lsp_status.on_attach,
  capabilities = lsp_status.capabilities
})
nvim_lsp.tsserver.setup({
  on_attach = lsp_status.on_attach,
  capabilities = lsp_status.capabilities
})

local protocol = require'vim.lsp.protocol'

local function texlab_attach()
    require'completion'.on_attach()
    protocol.SymbolKind = {
        'file';
        'sec';
        'fold';
        '';
        'class';
        'float';
        'lib';
        'field';
        'label';
        'enum';
        'misc';
        'cmd';
        'thm';
        'equ';
        'strg';
        'arg';
        '';
        '';
        'PhD';
        '';
        '';
        'item';
        'book';
        'artl';
        'part';
        'coll';
    }
    protocol.CompletionItemKind = {
        'string';
        '';
        '';
        '';
        'field';
        '';
        'class';
        'misc';
        '';
        'library';
        'thesis';
        'argument';
        '';
        '';
        'snippet';
        'color';
        'file';
        '';
        'folder';
        '';
        '';
        'book';
        'article';
        'part';
        'collect';
    }
end

nvim_lsp.texlab.setup{
    cmd = {vim.fn.stdpath("cache")..'/nvim_lsp/texlab'},
    on_attach=texlab_attach
}


nvim_lsp.pyls_ms.setup{on_attach=require'diagnostic'.on_attach}
nvim_lsp.gopls.setup{on_attach=require'diagnostic'.on_attach}
nvim_lsp.vimls.setup{on_attach=require'diagnostic'.on_attach}
nvim_lsp.jsonls.setup{on_attach=require'diagnostic'.on_attach}
nvim_lsp.tsserver.setup{on_attach=require'diagnostic'.on_attach}
nvim_lsp.rust_analyzer.setup{on_attach=require'diagnostic'.on_attach}
nvim_lsp.sumneko_lua.setup{on_attach=require'diagnostic'.on_attach}

nvim_lsp.pyls_ms.setup{on_attach=require'completion'.on_attach}
nvim_lsp.gopls.setup{on_attach=require'completion'.on_attach}
nvim_lsp.vimls.setup{on_attach=require'completion'.on_attach}
nvim_lsp.jsonls.setup{on_attach=require'completion'.on_attach}
nvim_lsp.tsserver.setup{on_attach=require'completion'.on_attach}
nvim_lsp.rust_analyzer.setup{on_attach=require'completion'.on_attach}
nvim_lsp.sumneko_lua.setup{on_attach=require'completion'.on_attach}
