require'nvim-treesitter.configs'.setup {
    highlight = {
        enable = true,
        keymaps = {
          init_selection = "gnn",
          node_incremental = "grn",
          scope_incremental = "grc",
          node_decremental = "grm",
        },
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
    ensure_installed = {"go", "rust", "markdown", "python", "lua", "yaml", "toml", "json", "typescript", "javascript"}
}
