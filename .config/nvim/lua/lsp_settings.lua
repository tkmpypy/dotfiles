local prequire = function(...)
	local status, lib = pcall(require, ...)
	if status then
		return lib
	end
	-- Library failed to load, so perhaps return `nil` or something?
	return nil
end

local vim = vim
local util = require 'lspconfig'.util

local custom_init = function(client)
	if client.config.flags then
		client.config.flags.allow_incremental_sync = false
	end
end

local custom_flags = function()
	return { debounce_text_changes = 300 }
end

local custom_attach = function(client, bufnr)
	-- Set autocommands conditional on server_capabilities
	if client.resolved_capabilities.document_highlight then
		vim.api.nvim_exec(
			[[
    augroup lsp_document_highlight
    autocmd! * <buffer>
    autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
    autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
    augroup END
    ]],
			false
		)
	end
end

-- Configure lua language server for neovim development
local lua_config = {
	settings = {
		Lua = {
			runtime = {
				-- LuaJIT in the case of Neovim
				version = "LuaJIT",
				path = vim.split(package.path, ";"),
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { "vim" },
			},
			-- workspace = {
			--   -- Make the server aware of Neovim runtime files
			--   library = {
			--     [vim.fn.expand('$VIMRUNTIME/lua')] = true,
			--     [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true
			--   }
			-- }
		},
	},
}

local dart_config = {
	init_options = {
		closingLabels = true,
		flutterOutline = true,
		onlyAnalyzeProjectsWithOpenFiles = true,
		outline = true,
		suggestFromUnimportedLibraries = true,
	},
}

local gopls_config = {
	init_options = {
		gofumpt = true,
		usePlaceholders = true,
		semanticTokens = true,
		staticcheck = true,
		experimentalPostfixCompletions = true,
		analyses = {
			nilness = true,
			shadow = true,
			unusedparams = true,
			unusedwrite = true,
			fieldalignment = true,
		},
		codelenses = { gc_details = true, tidy = true },
	},
}

local diagnosticls_config = {
  root_dir = function (fname)
    return util.root_pattern '.git'(fname) or util.path.dirname(fname)
  end,
	filetypes = {
		"javascript",
		"javascriptreact",
		"typescript",
		"typescriptreact",
		"go",
		"rust",
		"lua",
		"python",
    "proto",
	},
	init_options = {
		linters = {
			flake8 = {
				command = vim.fn.expand("$HOME/.pyenv/shims/flake8"),
				rootPatterns = { ".git" },
				debounce = 100,
				args = { "--format=%(row)d,%(col)d,%(code).1s,%(code)s: %(text)s", "-" },
				sourceName = "flake8",
				offsetLint = 0,
				offsetColumn = 0,
				formatLines = 1,
				formatPattern = {
					"(\\d+),(\\d+),([A-Z]),(.*)(\\r|\\n)*$",
					{ line = 1, column = 2, security = 3, message = { "[flake8]", 4 } },
				},
				securities = {
					W = "warning",
					E = "error",
					F = "error",
					C = "error",
					N = "error",
				},
			},
			eslint = {
				command = "./node_modules/.bin/eslint",
				rootPatterns = { ".git" },
				debounce = 300,
				args = { "--stdin", "--stdin-filename", "%filepath", "--format", "json" },
				sourceName = "eslint",
				parseJson = {
					errorsRoot = "[0].messages",
					line = "line",
					column = "column",
					endLine = "endLine",
					endColumn = "endColumn",
					message = "${message} [${ruleId}]",
					security = "severity",
				},
				securities = { [2] = "error", [1] = "warning" },
			},
			golangci = {
				command = "golangci-lint",
				sourceName = "golangci-lint",
				rootPatterns = { ".git", "go.mod" },
				debounce = 300,
				args = { "run", "--out-format", "json", "--fast" },
				parseJson = {
					sourceName = "Pos.Filename",
					sourceNameFilter = true,
					errorsRoot = "Issues",
					line = "Pos.Line",
					column = "Pos.Column",
					message = "${Text} [${FromLinter}]",
				},
			},
      buf = {
				command = "buf",
				sourceName = "buf",
				args = { "lint", "--path", vim.api.nvim_buf_get_name(0), "--error-format=text" },
				rootPatterns = { "buf.yaml", "buf.yml" },
				offsetLint = 0,
				offsetColumn = 0,
				formatLines = 1,
				formatPattern = {
					"([a-zA-Z|\\/|\\.]*):(\\d+):(\\d+):(.*)*$",
					{ sourceName = 1, line = 2, column = 3, message = 4 },
				},
      },
		},
		filetypes = {
			javascript = "eslint",
			javascriptreact = "eslint",
			typescript = "eslint",
			typescriptreact = "eslint",
			dart = "dartanalyzer",
			go = "golangci",
			python = "flake8",
			proto = "buf",
		},
	},
}

-- config that activates keymaps and enables snippet support
local make_config = function()
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	capabilities.textDocument.completion.completionItem.snippetSupport = true
	capabilities.textDocument.completion.completionItem.preselectSupport = true
	capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
	capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
	capabilities.textDocument.completion.completionItem.deprecatedSupport = true
	capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
	capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
	capabilities.textDocument.completion.completionItem.resolveSupport = {
		properties = { "documentation", "detail", "additionalTextEdits" },
	}
	return {
		-- enable snippet support
		capabilities = capabilities,
		-- map buffer local keybindings when the language server attaches
		on_attach = custom_attach,
		on_init = custom_init,
		flags = custom_flags(),
	}
end

local function setup_servers_use_nvim_lsp_installer()
	local lsp_installer = require("nvim-lsp-installer")

	lsp_installer.on_server_ready(function(server)
		local opts = {}
		local config = make_config()

		-- (optional) Customize the options passed to the server
		-- if server.name == "tsserver" then
		--     opts.root_dir = function() ... end
		-- end
		if server.name == "sumneko_lua" then
			config.settings = lua_config.settings
		elseif server.name == "gopls" then
			config.init_options = gopls_config.init_options
		elseif server.name == "diagnosticls" then
			config.init_options = diagnosticls_config.init_options
			config.filetypes = diagnosticls_config.filetypes
      config.root_dir = diagnosticls_config.root_dir
		end

		-- This setup() function is exactly the same as lspconfig's setup function (:help lspconfig-quickstart)
		opts = config
		server:setup(opts)
		vim.cmd([[ do User LspAttachBuffers ]])
	end)
end

setup_servers_use_nvim_lsp_installer()

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
	-- Enable underline, use default values
	underline = true,
	-- Enable virtual text, override spacing to 4
	virtual_text = { spacing = 2, prefix = "»" },
	-- Use a function to dynamically turn signs off
	-- and on, using buffer local variables
	-- signs = function(bufnr, client_id)
	--   local ok, result = pcall(vim.api.nvim_buf_get_var, bufnr, 'show_signs')
	--   -- No buffer local variable set, so just enable by default
	--   if not ok then
	--     return true
	--   end
	--
	--   return result
	-- end,
	signs = { priority = 20 },
	-- Disable a feature
	update_in_insert = false,
})
