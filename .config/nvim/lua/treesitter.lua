require("nvim-treesitter.configs").setup {
  yati = { enable = true },
  highlight = {
    enable = true,
    disable = {'org'},
    additional_vim_regex_highlighting = {'org'},
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
  indent = {
    enable = false,
  },
  refactor = {
    highlight_defintions = { enable = true },
    smart_rename = { enable = false },
    navigation = { enable = false },
  },
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
    -- "markdown"
    "org"
  },
}
