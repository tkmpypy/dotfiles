-- https://github.com/davidsierradz/cmp-conventionalcommits
-- https://github.com/davidsierradz/cmp-conventionalcommits/blob/master/lua/cmp-conventionalcommits/init.lua
local source = {}

local typesDict = {}
typesDict["build"] = {
  label = "build",
  documentation = "Changes that affect the build system or external dependencies",
}
typesDict["chore"] = {
  label = "chore",
  documentation = "Other changes that dont modify src or test files",
}
typesDict["ci"] = {
  label = "ci",
  documentation = "Changes to our CI configuration files and scripts",
}
typesDict["docs"] = {
  label = "docs",
  documentation = "Documentation only changes",
}
typesDict["feat"] = { label = "feat", documentation = "A new feature" }
typesDict["fix"] = { label = "fix", documentation = "A bug fix" }
typesDict["perf"] = {
  label = "perf",
  documentation = "A code change that improves performance",
}
typesDict["refactor"] = {
  label = "refactor",
  documentation = "A code change that neither fixes a bug nor adds a feature",
}
typesDict["revert"] = {
  label = "revert",
  documentation = "Reverts a previous commit",
}
typesDict["style"] = {
  label = "style",
  documentation = "Changes that do not affect the meaning of the code",
}
typesDict["test"] = {
  label = "test",
  documentation = "Adding missing tests or correcting existing tests",
}

function source:new()
  source.types = {
    typesDict["build"],
    typesDict["chore"],
    typesDict["ci"],
    typesDict["docs"],
    typesDict["feat"],
    typesDict["fix"],
    typesDict["perf"],
    typesDict["refactor"],
    typesDict["revert"],
    typesDict["style"],
    typesDict["test"],
  }
  return setmetatable({}, { __index = source })
end
---Return this source is available in current context or not. (Optional)
---@return boolean
-- function source:is_available()
--   return true
-- end
---Return the debug name of this source. (Optional)
---@return string
function source:get_debug_name()
  return "conventionalprefix"
end
---Return keyword pattern for triggering completion. (Optional)
---If this is ommited, nvim-cmp will use default keyword pattern. See |cmp-config.completion.keyword_pattern|
---@return string
function source:get_keyword_pattern()
  return [[\k\+]]
end

local function candidates(entries)
  local items = {}
  for k, v in ipairs(entries) do
    items[k] = {
      label = v.label,
      kind = require("cmp").lsp.CompletionItemKind.Keyword,
      documentation = v.documentation,
    }
  end
  return items
end

function source:complete(_, callback)
  callback(candidates(self.types))
end

---Register custom source to nvim-cmp.
require("cmp").register_source("conventionalprefix", source.new())
