local M = {}
local base_url = "https://api.openai.com/v1"

---@alias CommitMsgType "simple"|"conventional"|

---@class CommitMsgGen.App
---@field config CommitMsgGen.UserConfig
---@field init fun(self:CommitMsgGen.App)
---@field generate_commit_msg fun(self:CommitMsgGen.App): string

---@class CommitMsgGen.UserConfig
---@field model? string
---@field msg_type? CommitMsgType

---@class CommitMsgGen.Config
---@field model string
---@field msg_type CommitMsgType

---@class OpenaiCompletion.ChatCompletionRequest
---@field model string
---@field messages OpenaiCompletion.ChatCompletionMessage[]

---@class OpenaiCompletion.ChatCompletionMessage
---@field role string
---@field content string

---@class OpenaiCompletion.ChatCompletionChoice
---@field finish_reason string
---@field index number
---@field message OpenaiCompletion.ChatCompletionMessage

---@class OpenaiCompletion.ChatCompletionUsage
---@field completion_tokens number
---@field prompt_tokens number
---@field total_tokens number

---@class OpenaiCompletion.ChatCompletionResponse
---@field choices OpenaiCompletion.ChatCompletionChoice[]
---@field created number
---@field id string
---@field model string
---@field object string
---@field usage OpenaiCompletion.ChatCompletionUsage

---@class OpenAI.Client
---@field api_key string
---@field build_prompt fun(self: OpenAI.Client, type: CommitMsgType, diff: string): string
---@field request_completions fun(self:OpenAI.Client, params:OpenaiCompletion.ChatCompletionRequest, on_success:fun(resp:table), on_error:fun(err:table), on_exit:fun())

local app_name = "CommitMsgGen"

---`vim.notify` wrapper
---@param msg string
---@param level number | nil
local notify = function(msg, level)
  vim.notify(msg, level, { title = app_name })
end

---check git staged
---@return boolean
local is_not_staged = function()
  local staged = vim.fn.trim(vim.fn.system({
    "git",
    "diff",
    "--name-only",
    "--staged",
  }))

  return #staged == 0
end

---do `git diff HEAD`
---@return string?
local get_git_diff = function()
  local r = vim.fn.system({
    "git",
    "diff",
    "--staged",
  })

  return r
end

---get api key
---@return string?
local get_api_token = function()
  return os.getenv("OPENAI_API_KEY")
end

---create command
---@param cmd string
---@param args string[]
---@return string
local create_cmd = function(cmd, args)
  local c = cmd
  local a = table.concat(args, " ")
  return c .. " " .. a
end

local default_config = {
  model = "gpt-3.5-turbo-16k",
  ---@type CommitMsgType
  msg_type = "conventional",
}

local Config = {}

---create CommitMsgGen.Config
---@param conf CommitMsgGen.UserConfig
---@return CommitMsgGen.Config
Config.new = function(conf)
  local c = default_config
  if conf then
    c = vim.tbl_deep_extend("force", default_config, conf)
  end
  setmetatable(c, { __index = Config })

  return c
end

local OpenAI = {}

---@return OpenAI.Client
OpenAI.new = function(api_key)
  local obj = {
    api_key = api_key,
  }
  setmetatable(obj, { __index = OpenAI })

  return obj
end

---comment
---@param _ OpenAI.Client
---@param type CommitMsgType
---@param diff string
---@return string
OpenAI.build_prompt = function(_, type, diff)
  local s = {
    "Please tell us the appropriate commit message from the following git diff",
    "Result format is only commit message",
  }
  if type == "conventional" then
    s[#s + 1] = "Commit message should be in conventional commit format"
  end

  s[#s + 1] = string.format("```diff\n%s```\n",diff)

  return vim.fn.join(s, "\n")
end

OpenAI.request_completions = function(self, params, on_success, on_error, on_exit)
  local encoded = vim.fn.json_encode(params)
  encoded = vim.fn.shellescape(encoded)
  local cmd = create_cmd("curl", {
    "-sS",
    "--no-buffer",
    base_url .. "/chat/completions",
    "-H",
    "'Content-Type: application/json'",
    "-H",
    "'Authorization: Bearer " .. self.api_key .. "'",
    "-d",
    encoded,
  })

  vim.fn.jobstart(cmd, {
    on_stdout = function(_, d, _)
      local s = vim.fn.join(d, "")
      if #s > 0 then
        on_success(s)
      end
    end,
    on_stderr = function(_, d, _)
      if #d > 0 and #d[1] > 0 then
        on_error(d)
      end
    end,
    on_exit = function()
      on_exit()
    end,
  })
end

---on success callback
---@param resp string
local on_success = function(resp)
  ---@type OpenaiCompletion.ChatCompletionResponse
  local r = vim.json.decode(resp)
  ---@param choice OpenaiCompletion.ChatCompletionChoice
  local t = vim.tbl_map(function(choice)
    return vim.split(choice.message.content, "\n")
  end, r.choices)
  vim.api.nvim_put(vim.tbl_flatten(t), "l", false, true)
end
---on error callback
---@param err table
local on_error = function(err)
  notify(vim.inspect(err), vim.log.levels.ERROR)
end
---on exit callback
local on_exit = function() end

local App = {}

---create app
---@param conf CommitMsgGen.UserConfig
---@return CommitMsgGen.App
App.new = function(conf)
  local obj = {
    config = Config.new(conf),
  }
  setmetatable(obj, { __index = App })

  return obj
end

---initialize plugin
--- - create command
---@param _ CommitMsgGen.App
App.init = function(_)
  vim.api.nvim_create_user_command("CommitMsgGen", function()
    M.app:generate_commit_msg()
  end, { bang = false })
end

---entry point function
---@param self CommitMsgGen.App
App.generate_commit_msg = function(self)
  local key = get_api_token()
  if not key then
    notify("you must set 'OPENAI_API_KEY'", vim.log.levels.ERROR)
    return
  end

  if is_not_staged() then
    notify("not staged", vim.log.levels.WARN)
    return
  end

  local diff = get_git_diff()
  if diff == nil then
    notify("you don't have git diff", vim.log.levels.WARN)
    return
  end

  local api = OpenAI.new(key)
  local prompt = api:build_prompt("conventional", diff)

  api:request_completions({
    model = self.config.model,
    messages = {
      {
        role = "user",
        content = prompt,
      },
    },
  }, on_success, on_error, on_exit)
end

M.setup = function(opts)
  local app = App.new(opts)
  M.app = app

  app:init()
end

return M
