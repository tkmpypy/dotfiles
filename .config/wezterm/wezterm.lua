-- https://wezfurlong.org/wezterm/config/keys.html
local wezterm = require "wezterm"
return {
  -- term = "xterm-256color",
  term = "wezterm",
  -- font = wezterm.font("JetBrainsMonoNL Nerd Font", { weight = "Medium" }),
  font = wezterm.font("PlemolJP Console NF", {weight = "Medium"}),
  -- font = wezterm.font("UDEV Gothic NF", {weight = "Regular"}),
  -- font = wezterm.font("FirgeNerd", {weight = "Regular"}),

  font_size = 15,
  freetype_load_target = "Normal",
  enable_scroll_bar = false,
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  enable_tab_bar = false,
  color_scheme = "nord",
  leader = { key = ",", mods = "CTRL", timeout_milliseconds = 1000 },
  keys = {
    { key = "+", mods = "LEADER|CTRL", action = "IncreaseFontSize" },
    { key = "-", mods = "LEADER|CTRL", action = "DecreaseFontSize" },
    { key = "=", mods = "LEADER|CTRL", action = "ResetFontSize" },
    { key = "Enter", mods = "LEADER|ALT", action = "ToggleFullScreen" },
    { key = "r", mods = "LEADER|ALT", action = "ReloadConfiguration" },
  },
}
