local awful = require("awful")

local _M = {
  terminal = "wezterm connect unix",
  term2 = "wezterm",
  browser = "firefox",
  launcher = "rofi -show combi",
  bookmarks = "rofi -show bookmarks -sidebar-mode",
  windowswitcher = "rofi -show window -sidebar-mode",
  powerMenu = "rofi -show power-menu -modi power-menu:rofi-power-menu",
  screenlock = "xflock4",
  screenshot = "flameshot gui",
  editor = "emacsclient -c",
  filemanager = "Thunar",
  browser2 = "brave",
  device = "desktop", -- valid options: desktop | laptop
  modkey = "Mod4",
  -- to be spawned all at once with a keybind:
  start_work_apps = function()
    awful.spawn("firefox")
    awful.spawn("zotero")
    awful.spawn("discord")
    awful.spawn("thunderbird")
    awful.spawn("emacsclient -c '~/Documents/Notes/working-todo.org'", { tag = awful.screen.focused().tags[2] })
  end,

  -- for multi-monitor purposes:
  screen1 = screen.primary,
  screen2_maybe = awful.screen.getbycoord(2560, 0) or screen.primary,

}
return _M
