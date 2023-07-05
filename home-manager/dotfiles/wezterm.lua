-- Pull in the wezterm API
local wezterm = require 'wezterm'
local act = wezterm.action

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This table will hold the configuration.
local config = {

  font_size = 16.0,
  font = wezterm.font "Mononoki",

  color_scheme = "Catppuccin Macchiato",

  -- tab bar
  hide_tab_bar_if_only_one_tab = false,
  use_fancy_tab_bar = false,
  window_frame = {
    font = wezterm.font { family = "Mononoki" },
    font_size = 16,
  },

  adjust_window_size_when_changing_font_size = false,

  window_padding = {
    left = 3, right = 3, top = 3, bottom = 3,
  },

  default_cursor_style = "SteadyBar",

  -- multiplexing:
  unix_domains = {
    { name = 'unix', },
  },

  default_gui_startup_args = { 'connect', 'unix' },


  -- Keybinds

  disable_default_key_bindings = true,

  leader = { key = "Space", mods = "CTRL" },
  keys = {
    { key = "b",     mods = "LEADER",       action = wezterm.action { SplitVertical = { domain = "CurrentPaneDomain" } } },
    { key = "v",     mods = "LEADER",       action = wezterm.action { SplitHorizontal = { domain = "CurrentPaneDomain" } } },
    { key = "n",     mods = "LEADER",       action = wezterm.action { SpawnTab = "CurrentPaneDomain" } },
    { key = "h",     mods = "LEADER",       action = wezterm.action { ActivatePaneDirection = "Left" } },
    { key = "j",     mods = "LEADER",       action = wezterm.action { ActivatePaneDirection = "Down" } },
    { key = "k",     mods = "LEADER",       action = wezterm.action { ActivatePaneDirection = "Up" } },
    { key = "l",     mods = "LEADER",       action = wezterm.action { ActivatePaneDirection = "Right" } },
    { key = "H",     mods = "LEADER|SHIFT", action = wezterm.action { AdjustPaneSize = { "Left", 5 } } },
    { key = "J",     mods = "LEADER|SHIFT", action = wezterm.action { AdjustPaneSize = { "Down", 5 } } },
    { key = "K",     mods = "LEADER|SHIFT", action = wezterm.action { AdjustPaneSize = { "Up", 5 } } },
    { key = "L",     mods = "LEADER|SHIFT", action = wezterm.action { AdjustPaneSize = { "Right", 5 } } },
    { key = "1",     mods = "LEADER",       action = wezterm.action { ActivateTab = 0 } },
    { key = "2",     mods = "LEADER",       action = wezterm.action { ActivateTab = 1 } },
    { key = "3",     mods = "LEADER",       action = wezterm.action { ActivateTab = 2 } },
    { key = "4",     mods = "LEADER",       action = wezterm.action { ActivateTab = 3 } },
    { key = "5",     mods = "LEADER",       action = wezterm.action { ActivateTab = 4 } },
    { key = "6",     mods = "LEADER",       action = wezterm.action { ActivateTab = 5 } },
    { key = "7",     mods = "LEADER",       action = wezterm.action { ActivateTab = 6 } },
    { key = "8",     mods = "LEADER",       action = wezterm.action { ActivateTab = 7 } },
    { key = "9",     mods = "LEADER",       action = wezterm.action { ActivateTab = 8 } },
    { key = "x",     mods = "LEADER",       action = wezterm.action { CloseCurrentPane = { confirm = true } } },
    { key = "v",     mods = "SHIFT|CTRL",   action = wezterm.action.PasteFrom 'Clipboard' },
    { key = "c",     mods = "SHIFT|CTRL",   action = wezterm.action.CopyTo 'Clipboard' },
    { key = "Space", mods = "LEADER",       action = wezterm.action.ActivateCopyMode },
    { key = "=",     mods = "CTRL",         action = wezterm.action.IncreaseFontSize },
    { key = "-",     mods = "CTRL",         action = wezterm.action.DecreaseFontSize },


  },

  key_tables = {
    copy_mode = {
      { key = 'Tab',    mods = 'NONE',  action = act.CopyMode 'MoveForwardWord' },
      { key = 'Tab',    mods = 'SHIFT', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'Enter',  mods = 'NONE',  action = act.CopyMode 'MoveToStartOfNextLine' },
      { key = 'Escape', mods = 'NONE',  action = act.CopyMode 'Close' },
      { key = '0',      mods = 'NONE',  action = act.CopyMode 'MoveToStartOfLine' },
      { key = 'G',      mods = 'SHIFT', action = act.CopyMode 'MoveToScrollbackBottom' },
      { key = 'H',      mods = 'SHIFT', action = act.CopyMode 'MoveToViewportTop' },
      { key = 'L',      mods = 'SHIFT', action = act.CopyMode 'MoveToViewportBottom' },
      { key = 'M',      mods = 'SHIFT', action = act.CopyMode 'MoveToViewportMiddle' },
      { key = 'O',      mods = 'SHIFT', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
      { key = 'T',      mods = 'SHIFT', action = act.CopyMode { JumpBackward = { prev_char = true } } },
      { key = 'g',      mods = 'NONE',  action = act.CopyMode 'MoveToScrollbackTop' },
      { key = 'h',      mods = 'NONE',  action = act.CopyMode 'MoveLeft' },
      { key = 'j',      mods = 'NONE',  action = act.CopyMode 'MoveDown' },
      { key = 'k',      mods = 'NONE',  action = act.CopyMode 'MoveUp' },
      { key = 'l',      mods = 'NONE',  action = act.CopyMode 'MoveRight' },
      { key = 'm',      mods = 'ALT',   action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = 'q',      mods = 'NONE',  action = act.CopyMode 'Close' },
      { key = 'V',      mods = 'SHIFT', action = act.CopyMode { SetSelectionMode = 'Line' } },
      { key = 'v',      mods = 'NONE',  action = act.CopyMode { SetSelectionMode = 'Cell' } },
      { key = 'v',      mods = 'CTRL',  action = act.CopyMode { SetSelectionMode = 'Block' } },
      { key = 'w',      mods = 'NONE',  action = act.CopyMode 'MoveForwardWord' },
      {
        key = 'y',
        mods = 'NONE',
        action = act.Multiple { { CopyTo = 'ClipboardAndPrimarySelection' }, {
          CopyMode = 'Close' } }
      },
      { key = 'PageUp',     mods = 'NONE', action = act.CopyMode 'PageUp' },
      { key = 'PageDown',   mods = 'NONE', action = act.CopyMode 'PageDown' },
      { key = 'End',        mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = 'Home',       mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
      { key = 'LeftArrow',  mods = 'NONE', action = act.CopyMode 'MoveLeft' },
      { key = 'RightArrow', mods = 'NONE', action = act.CopyMode 'MoveRight' },
      { key = 'UpArrow',    mods = 'NONE', action = act.CopyMode 'MoveUp' },
      { key = 'DownArrow',  mods = 'NONE', action = act.CopyMode 'MoveDown' },
    },
  },


}


-- This is where you actually apply your config choices

-- and finally, return the configuration to wezterm
return config
