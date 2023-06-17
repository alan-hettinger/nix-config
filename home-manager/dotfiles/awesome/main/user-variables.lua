local home = os.getenv("HOME")

local _M = {
  terminal = "alacritty",
  term2 = "rofi-tmux",
  browser = "firefox",
  launcher = "rofi -show drun -sidebar-mode",
  bookmarks = "rofi -show bookmarks -sidebar-mode",
  windowswitcher = "rofi -show window -sidebar-mode",
  screenlock = "/home/alan/.config/awesome/lock.sh",
  editor = "emacsclient -c",
  filemanager = "dolphin",
  browser2 = "brave",
  device = "desktop", -- valid options: desktop | laptop
  modkey = "Mod4",
}
return _M
