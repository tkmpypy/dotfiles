local bo = vim.bo

vim.api.nvim_create_autocmd({ "TextYankPost" }, {
  pattern = "*",
  callback = function(_)
    vim.highlight.on_yank({ timeout = 300 })
  end,
})
vim.api.nvim_create_autocmd({ "Syntax" }, {
  pattern = "*",
  callback = function(_)
    vim.cmd([[
      syn sync minlines=500 maxlines=1000
    ]])
  end,
})
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = {
    "bash",
    "sh",
    "zsh",
    "dart",
    "vue",
    "typescript",
    "typescriptreact",
    "javascript",
    "javascriptreact",
    "lua",
    "yaml",
    "json",
    "proto",
    "markdown",
    "vim",
  },
  callback = function(_)
    bo.ts = 2
    bo.sw = 2
  end,
})
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = "go",
  callback = function(_)
    bo.expandtab = true
  end,
})
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = "python",
  callback = function(_)
    bo.ts = 4
    bo.sw = 4
  end,
})
