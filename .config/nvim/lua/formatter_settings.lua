local prettier = function(parser)
  local args = {}
  if parser == nil then
    args = { "-w", "--parser", "babel" }
  else
    args = { "-w", "--parser", parser }
  end
	return function()
    return {
      exe = vim.fn.expand("$HOME/.anyenv/envs/nodenv/shims/prettier"),
      args = args,
      stdin = true,
    }
  end
end

local eslint = function()
	return {
		exe = "./node_modules/.bin/eslint",
		args = { "--fix", "--no-ignore", "--no-color", "--stdin-filename", vim.api.nvim_buf_get_name(0) },
		stdin = false,
	}
end

require("formatter").setup({
	logging = true,
	filetype = {
		terraform = {
			function()
				return {
					exe = "terraform",
					args = { "fmt", "-" },
					stdin = true,
				}
			end,
		},
		go = {
			function()
				return {
					exe = "gofmt",
					stdin = true,
					tempfile_postfix = ".tmp",
				}
			end,
			function()
				return {
					exe = "goimports",
					stdin = true,
					tempfile_postfix = ".tmp",
				}
			end,
		},
		lua = {
			function()
				return {
					exe = "stylua",
					stdin = true,
					args = { "--stdin-filepath", vim.api.nvim_buf_get_name(0), "-" },
				}
			end,
		},
		javascript = { prettier(nil), eslint },
		javascriptreact = { prettier(nil), eslint },
		typescript = { prettier(nil), eslint },
		typescriptreact = { prettier(nil), eslint },
		json = { prettier("json") },
		markdown = { prettier("mdx") },
		yaml = { prettier("yaml") },
		rust = {
			function()
				return {
					exe = "rustfmt",
					stdin = true,
					args = { "--emit=stdout", "--edition", "2018"},
				}
			end,
		},
		python = {
			function()
				return {
					exe = vim.fn.expand("$HOME/.anyenv/envs/pyenv/shims/autopep8"),
					stdin = true,
					args = { "--aggressive", "--experimental", "-" },
				}
			end,
		},
		dart = {
			function()
				return {
					exe = "dartfmt",
					stdin = true,
					args = { "--fix", "-w", vim.api.nvim_buf_get_name(0) },
				}
			end,
		},
		sh = {
			function()
				return {
					exe = "shfmt",
					stdin = true,
					args = { "-i", 2 },
				}
			end,
		},
	},
})
