-- https://wezfurlong.org/wezterm/config/keys.html
local wezterm = require "wezterm"
local bg_transparent_enabled = true
local bg_transparent_opacity = 0.7

return {
  -- term = "xterm-256color",
  term = "wezterm",
  -- font = wezterm.font("JetBrainsMonoNL Nerd Font", { weight = "Medium" }),
  font = wezterm.font("PlemolJP Console NF", { weight = "Medium" }),
  -- font = wezterm.font("UDEV Gothic NF", {weight = "Regular"}),
  -- font = wezterm.font("FirgeNerd", {weight = "Regular"}),

  font_size = 15,
  adjust_window_size_when_changing_font_size = false,
  freetype_load_target = "Normal",
  enable_scroll_bar = false,
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  window_background_opacity = bg_transparent_opacity,
  enable_tab_bar = false,
  color_scheme = "nord",
  leader = { key = ",", mods = "CTRL", timeout_milliseconds = 1000 },
  keys = {
    { key = "+", mods = "LEADER|CTRL", action = "IncreaseFontSize" },
    { key = "-", mods = "LEADER|CTRL", action = "DecreaseFontSize" },
    { key = "0", mods = "LEADER|CTRL", action = "ResetFontSize" },
    { key = "Enter", mods = "LEADER|ALT", action = "ToggleFullScreen" },
    { key = "r", mods = "LEADER|CTRL", action = "ReloadConfiguration" },
    {
      key = "t",
      mods = "ALT",
      action = wezterm.action_callback(function(win, pane)
        bg_transparent_enabled = not bg_transparent_enabled
        local overrides = win:get_config_overrides() or {}
        if bg_transparent_enabled then
          overrides.window_background_opacity = bg_transparent_opacity
        else
          overrides.window_background_opacity = 1.0
        end
        win:set_config_overrides(overrides)
      end),
    },
  },
}
