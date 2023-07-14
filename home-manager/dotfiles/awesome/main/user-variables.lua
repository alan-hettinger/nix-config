local home = os.getenv("HOME")
local awful = require("awful")

local _M = {
  terminal = "wezterm",
  term2 = "wezterm connect unix",
  browser = "firefox",
  launcher = "rofi -show combi",
  bookmarks = "rofi -show bookmarks -sidebar-mode",
  windowswitcher = "rofi -show window -sidebar-mode",
  powerMenu = "rofi -show power-menu -modi power-menu:rofi-power-menu",
  screenlock = "xflock4",
  screenshot = "flameshot gui",
  editor = "emacsclient -c",
  filemanager = "thunar",
  browser2 = "brave",
  device = "desktop", -- valid options: desktop | laptop
  modkey = "Mod4",
  -- to be spawned all at once with a keybind:
  start_work_apps = function()
    awful.spawn("firefox")
    awful.spawn("zotero")
    awful.spawn("discord")
    awful.spawn("thunderbird")
    awful.spawn("emacsclient -c", { tag = awful.screen.focused().tags[2] })
  end
}
return _M
