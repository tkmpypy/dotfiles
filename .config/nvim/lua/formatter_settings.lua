local prettier = function()
  return {
    exe = 'prettier',
    args = {'--stdin-filepath', vim.api.nvim_buf_get_name(0), '--single-quote'},
    stdin = true
  }
end

local eslint = function()
  return {exe = './node_modules/.bin/eslint', args = {'--fix'}, stdin = true}
end

require('formatter').setup({
  logging = true,
  filetype = {
    go = {
      function()
        return {
          exe = 'gofmt',
          args = {'-w'},
          stdin = true,
          tempfile_postfix = ".tmp"
        }
      end, function()
        return {
          exe = 'goimports',
          stdin = true,
          args = {'-w'},
          tempfile_postfix = ".tmp"
        }
      end
    },
    lua = {
      function()
        return {
          exe = 'lua-format',
          stdin = true,
          args = {'--indent-width=2', vim.api.nvim_buf_get_name(0)}
        }
      end
    },
    javascript = {prettier, eslint},
    javascriptreact = {prettier, eslint},
    typescript = {prettier, eslint},
    typescriptreact = {prettier, eslint},
    json = {prettier},
    markdown = {prettier},
    rust = {
      function()
        return {
          exe = 'rustfmt',
          stdin = true,
          args = {'--edition', '2018', vim.api.nvim_buf_get_name(0)}
        }
      end
    },
    python = {
      function()
        return {
          exe = vim.fn.expand('$HOME/.pyenv/shims/autopep8'),
          stdin = true,
          args = {'--aggressive', '--experimental', '-'}
        }
      end
    },
    dart = {
      function()
        return {
          exe = 'dartfmt',
          stdin = true,
          args = {'--fix', '-w', vim.api.nvim_buf_get_name(0)}
        }
      end
    }
  }
})

