require'format'.setup {
    ["*"] = {
        {cmd = {"sed -i 's/[ \t]*$//'"}} -- remove trailing whitespace
    },
    lua = {
        {
            cmd = {
                function(file)
                    return string.format("lua-format -i %s", file)
                end
            }
        }
    },
    go = {{cmd = {"gofmt -w", "goimports -w"}, tempfile_postfix = ".tmp"}},
    javascript = {{cmd = {"prettier -w", "./node_modules/.bin/eslint --fix"}}},
    javascriptreact = {
        {cmd = {"prettier -w", "./node_modules/.bin/eslint --fix"}}
    },
    typescript = {{cmd = {"prettier -w", "./node_modules/.bin/eslint --fix"}}},
    typescriptreact = {
        {cmd = {"prettier -w", "./node_modules/.bin/eslint --fix"}}
    },
    markdown = {
        {cmd = {"prettier -w"}}, {
            cmd = {"black"},
            start_pattern = "^```python$",
            end_pattern = "^```$",
            target = "current"
        }
    },
    rust = {
        {
            cmd = {
                function(file)
                    return string.format("rustfmt --edition 2018 %s", file)
                end
            }
        }
    },
    dart = {
        {
            cmd = {
                function(file)
                    return string.format("dartfmt --fix -w %s", file)
                end
            }
        }
    },
    python = {
        {
            cmd = {
                function(file)
                    return string.format("autopep8 --in-place --aggressive %s", file)
                end
            }
        }
    }
}
