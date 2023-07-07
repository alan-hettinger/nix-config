local home = os.getenv("HOME")

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
}
return _M
