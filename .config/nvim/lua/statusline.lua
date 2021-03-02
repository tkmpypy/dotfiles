local gl = require('galaxyline')
local lspclient = require('galaxyline.provider_lsp')
local gls = gl.section
gl.short_line_list = {'LuaTree','vista','dbui'}

local one_dark_colors = {
  bg = "#2e3440",
  fg = "#81a1c1",
  line_bg = "#2e3440",
  fg_green = "#6d96a5",
  yellow = "#fabd2f",
  cyan = "#008080",
  darkblue = "#081633",
  green = "#608B4E",
  orange = "#FF8800",
  purple = "#5d4d7a",
  magenta = "#d16d9e",
  grey = "#c0c0c0",
  blue = "#569CD6",
  red = "#D16969"
}

local nord_colors = {
  bg = "NONE",
  -- bg = "#2E3440",
  fg = "#81A1C1",
  line_bg = "NONE",
  -- line_bg = "#2E3440",
  fg_green = "#8FBCBB",
  yellow = "#EBCB8B",
  cyan = "#A3BE8C",
  darkblue = "#81A1C1",
  green = "#8FBCBB",
  orange = "#D08770",
  purple = "#B48EAD",
  magenta = "#BF616A",
  gray = "#616E88",
  blue = "#5E81AC",
  red = "#BF616A"
}

local find_git_root = function()
  local path = vim.fn.expand('%:p:h')
  local get_git_dir = require('galaxyline.provider_vcs').get_git_dir
  return get_git_dir(path)
end

local buffer_not_empty = function()
  if vim.fn.empty(vim.fn.expand("%:t")) ~= 1 then
    return true
  end
  return false
end

local checkwidth = function()
  local squeeze_width = vim.fn.winwidth(0) / 2
  if squeeze_width > 40 then
    return true
  end
  return false
end

gls.left[1] = {
  FirstElement = {
    -- provider = function() return '▊ ' end,
    provider = function()
      return "  "
    end,
    highlight = {nord_colors.blue, nord_colors.line_bg}
  }
}
gls.left[2] = {
  ViMode = {
    provider = function()
      -- auto change color according the vim mode
      local mode_color = {
        n = nord_colors.magenta,
        i = nord_colors.green,
        v = nord_colors.blue,
        [""] = nord_colors.blue,
        V = nord_colors.blue,
        c = nord_colors.red,
        no = nord_colors.magenta,
        s = nord_colors.orange,
        S = nord_colors.orange,
        [""] = nord_colors.orange,
        ic = nord_colors.yellow,
        R = nord_colors.purple,
        Rv = nord_colors.purple,
        cv = nord_colors.red,
        ce = nord_colors.red,
        r = nord_colors.cyan,
        rm = nord_colors.cyan,
        ["r?"] = nord_colors.cyan,
        ["!"] = nord_colors.red,
        t = nord_colors.red
      }
      vim.cmd("hi GalaxyViMode guifg=" .. mode_color[vim.fn.mode()])
      return "    "
    end,
    highlight = {nord_colors.red, nord_colors.line_bg, "bold"}
  }
}
gls.left[3] = {
  FileIcon = {
    provider = "FileIcon",
    condition = buffer_not_empty,
    highlight = {require("galaxyline.provider_fileinfo").get_file_icon_color, nord_colors.line_bg}
  }
}
gls.left[4] = {
  FileName = {
    -- provider = "FileName",
    provider = function()
      return vim.fn.expand("%:F")
    end,
    condition = buffer_not_empty,
    separator = " ",
    separator_highlight = {nord_colors.purple, nord_colors.bg},
    highlight = {nord_colors.purple, nord_colors.line_bg, "bold"}
  }
}

gls.right[1] = {
  GetLspClient = {
    provider = "GetLspClient",
    condition = checkwidth,
    highlight = {nord_colors.darkblue, nord_colors.line_bg, "bold"}
  }
}
gls.right[2] = {
  GitIcon = {
    provider = function()
      return " "
    end,
    condition = require("galaxyline.provider_vcs").check_git_workspace,
    separator = " ",
    separator_highlight = {nord_colors.purple, nord_colors.bg},
    highlight = {nord_colors.orange, nord_colors.line_bg}
  }
}
gls.right[3] = {
  GitBranch = {
    provider = "GitBranch",
    condition = require("galaxyline.provider_vcs").check_git_workspace,
    separator = "",
    separator_highlight = {nord_colors.purple, nord_colors.bg},
    highlight = {nord_colors.orange, nord_colors.line_bg, "bold"}
  }
}

gls.right[4] = {
  DiffAdd = {
    provider = "DiffAdd",
    condition = checkwidth,
    icon = " ",
    highlight = {nord_colors.green, nord_colors.line_bg}
  }
}
gls.right[5] = {
  DiffModified = {
    provider = "DiffModified",
    condition = checkwidth,
    icon = " ",
    highlight = {nord_colors.yellow, nord_colors.line_bg}
  }
}
gls.right[6] = {
  DiffRemove = {
    provider = "DiffRemove",
    condition = checkwidth,
    icon = " ",
    highlight = {nord_colors.red, nord_colors.line_bg}
  }
}

gls.right[7] = {
  LineInfo = {
    provider = "LineColumn",
    separator = "",
    separator_highlight = {nord_colors.blue, nord_colors.line_bg},
    highlight = {nord_colors.gray, nord_colors.line_bg}
  }
}
-- section.right[7] = {
--   FileSize = {
--     provider = "FileSize",
--     separator = " ",
--     condition = buffer_not_empty,
--     separator_highlight = {nord_colors.blue, nord_colors.line_bg},
--     highlight = {nord_colors.fg, nord_colors.line_bg}
--   }
-- }

gls.right[8] = {
  DiagnosticError = {
    provider = "DiagnosticError",
    separator = " ",
    icon = " ",
    highlight = {nord_colors.red, nord_colors.line_bg},
    separator_highlight = {nord_colors.bg, nord_colors.bg}
  }
}
gls.right[9] = {
  DiagnosticWarn = {
    provider = "DiagnosticWarn",
    -- separator = " ",
    icon = " ",
    highlight = {nord_colors.yellow, nord_colors.line_bg},
    separator_highlight = {nord_colors.bg, nord_colors.bg}
  }
}

gls.right[10] = {
  DiagnosticInfo = {
    -- separator = " ",
    provider = "DiagnosticInfo",
    icon = " ",
    highlight = {nord_colors.green, nord_colors.line_bg},
    separator_highlight = {nord_colors.bg, nord_colors.bg}
  }
}

gls.right[11] = {
  DiagnosticHint = {
    provider = "DiagnosticHint",
    -- separator = " ",
    icon = " ",
    highlight = {nord_colors.blue, nord_colors.line_bg},
    separator_highlight = {nord_colors.bg, nord_colors.bg}
  }
}

gls.short_line_left[1] = {
  BufferType = {
    provider = "FileTypeName",
    separator = " ",
    separator_highlight = {"NONE", nord_colors.bg},
    highlight = {nord_colors.blue, nord_colors.bg, "bold"}
  }
}

gls.short_line_left[2] = {
  SFileName = {
    provider = function()
      local fileinfo = require("galaxyline.provider_fileinfo")
      local fname = fileinfo.get_current_file_name()
      for _, v in ipairs(gl.short_line_list) do
        if v == vim.bo.filetype then
          return ""
        end
      end
      return fname
    end,
    condition = buffer_not_empty,
    highlight = {nord_colors.white, nord_colors.bg, "bold"}
  }
}

gls.short_line_right[1] = {
  BufferIcon = {
    provider = "BufferIcon",
    highlight = {nord_colors.fg, nord_colors.bg}
  }
}


-- default theme
-- local colors = {
--   bg = '#282c34',
--   line_bg = '#21242b',
--   fg = '#c0c0c0',
--
--   yellow = '#fabd2f',
--   cyan = '#008080',
--   darkblue = '#081633',
--   green = '#afd700',
--   orange = '#FF8800',
--   purple = '#5d4d7a',
--   magenta = '#c678dd',
--   blue = '#51afef';
--   red = '#ec5f67'
-- }
-- gls.left[1] = {
--   FirstElement = {
--     provider = function() return '▊ ' end,
--     highlight = {colors.blue,colors.line_bg}
--   },
-- }
-- gls.left[2] = {
--   ViMode = {
--     provider = function()
--       -- auto change color according the vim mode
--       local mode_color = {n = colors.magenta, i = colors.green,v=colors.blue,[''] = colors.blue,V=colors.blue,
--                           c = colors.red,no = colors.magenta,s = colors.orange,S=colors.orange,
--                           [''] = colors.orange,ic = colors.yellow,R = colors.purple,Rv = colors.purple,
--                           cv = colors.red,ce=colors.red, r = colors.cyan,rm = colors.cyan, ['r?'] = colors.cyan,
--                           ['!']  = colors.red,t = colors.red}
--       vim.api.nvim_command('hi GalaxyViMode guifg='..mode_color[vim.fn.mode()])
--       return '  '
--     end,
--     highlight = {colors.red,colors.line_bg,'bold'},
--   },
-- }
-- gls.left[3] ={
--   FileIcon = {
--     provider = 'FileIcon',
--     condition = buffer_not_empty,
--     highlight = {require('galaxyline.provider_fileinfo').get_file_icon_color,colors.line_bg},
--   },
-- }
-- gls.left[4] = {
--   FileName = {
--     provider = {'FileName','FileSize'},
--     condition = buffer_not_empty,
--     highlight = {colors.fg,colors.line_bg,'bold'}
--   }
-- }
-- gls.left[5] = {
--   GitIcon = {
--     provider = function() return '  ' end,
--     condition = find_git_root,
--     highlight = {colors.orange,colors.line_bg},
--   }
-- }
-- gls.left[6] = {
--   GitBranch = {
--     provider = 'GitBranch',
--     condition = find_git_root,
--     highlight = {colors.fg,colors.line_bg,'bold'},
--   }
-- }
-- gls.left[7] = {
--   DiffAdd = {
--     provider = 'DiffAdd',
--     condition = checkwidth,
--     icon = ' ',
--     highlight = {colors.green,colors.line_bg},
--   }
-- }
-- gls.left[8] = {
--   DiffModified = {
--     provider = 'DiffModified',
--     condition = checkwidth,
--     icon = ' ',
--     highlight = {colors.orange,colors.line_bg},
--   }
-- }
-- gls.left[9] = {
--   DiffRemove = {
--     provider = 'DiffRemove',
--     condition = checkwidth,
--     icon = ' ',
--     highlight = {colors.red,colors.line_bg},
--   }
-- }
-- gls.left[10] = {
--   Space = {
--     provider = function () return ' ' end,
--     highlight = {colors.bg,colors.line_bg}
--   }
-- }
-- gls.left[11] = {
--   DiagnosticInfo = {
--     provider = 'DiagnosticInfo',
--     icon = '  ',
--     highlight = {colors.green,colors.bg}
--   }
-- }
-- gls.left[12] = {
--   Space = {
--     provider = function () return ' ' end,
--     highlight = {colors.bg,colors.line_bg}
--   }
-- }
-- gls.left[13] = {
--   DiagnosticHint = {
--     provider = 'DiagnosticHint',
--     icon = ' ﱥ ',
--     highlight = {colors.blue,colors.bg}
--   }
-- }
-- gls.left[14] = {
--   Space = {
--     provider = function () return ' ' end,
--     highlight = {colors.bg,colors.line_bg}
--   }
-- }
-- gls.left[15] = {
--   DiagnosticWarn = {
--     provider = 'DiagnosticWarn',
--     icon = '  ',
--     highlight = {colors.yellow,colors.bg}
--   }
-- }
-- gls.left[16] = {
--   Space = {
--     provider = function () return ' ' end,
--     highlight = {colors.bg,colors.line_bg}
--   }
-- }
-- gls.left[17] = {
--   DiagnosticError = {
--     provider = 'DiagnosticError',
--     icon = '  ',
--     highlight = {colors.red,colors.bg},
--   }
-- }
-- gls.right[1]= {
--   FileFormat = {
--     provider = 'FileFormat',
--     separator = ' ',
--     separator_highlight = {colors.bg,colors.line_bg},
--     highlight = {colors.fg,colors.line_bg},
--   }
-- }
-- gls.right[2] = {
--   LineInfo = {
--     provider = 'LineColumn',
--     separator = ' | ',
--     separator_highlight = {colors.blue,colors.line_bg},
--     highlight = {colors.fg,colors.line_bg},
--   },
-- }
-- gls.right[3] = {
--   PerCent = {
--     provider = 'LinePercent',
--     separator = ' ',
--     separator_highlight = {colors.line_bg,colors.line_bg},
--     highlight = {colors.fg,colors.darkblue},
--   }
-- }
-- gls.right[4] = {
--   ScrollBar = {
--     provider = 'ScrollBar',
--     highlight = {colors.blue,colors.purple},
--   }
-- }
-- gls.short_line_left[1] = {
--   BufferType = {
--     provider = 'FileTypeName',
--     separator = ' ',
--     separator_highlight = {colors.purple,colors.bg},
--     highlight = {colors.fg,colors.purple}
--   }
-- }
-- gls.short_line_right[1] = {
--   BufferIcon = {
--     provider= 'BufferIcon',
--     separator = ' ',
--     separator_highlight = {colors.purple,colors.bg},
--     highlight = {colors.fg,colors.purple}
--   }
-- }
