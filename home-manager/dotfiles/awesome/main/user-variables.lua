local home = os.getenv("HOME")

local hostname = os.getenv("HOSTNAME")

local hname1 = hostname:find("desktop")
local hname2 = hostname:find("laptop")
if not (hname1 == nil) and (hname2 == nil) then     -- desktop configuration
  local _M = {
    terminal = home .. "/.local/bin/rofi-tmux",
    term2 = "alacritty",
    browser = "firefox",
    launcher = "rofi -show drun -sidebar-mode",
    bookmarks = "rofi -show bookmarks -sidebar-mode",
    windowswitcher = "rofi -show window -sidebar-mode",
    screenlock = "/home/alan/.config/awesome/lock.sh",
    editor = "emacsclient -c -a 'emacs'",
    filemanager = "dolphin", -- change to dolphin when I switch to nixos
    browser2 = "flatpak run com.brave.Browser",
    device = "desktop",   -- valid options: desktop | laptop
    modkey = "Mod4",
  }
  return _M
elseif not (hname2 == nil) and (hname1 == nil) then -- laptop configuration
  local _M = {
    terminal = "alacritty",
    term2 = "alacritty -e fish",
    browser = "firefox",
    launcher = "rofi -show drun -sidebar-mode",
    bookmarks = "rofi -show bookmarks -sidebar-mode",
    windowswitcher = "rofi -show window -sidebar-mode",
    screenlock = "/home/alan/.config/awesome/lock.sh",
    editor = "emacsclient -c -a 'emacs'",
    filemanager = "dolphin",
    browser2 = "flatpak run com.brave.Browser",
    device = "laptop", -- valid options: desktop | laptop
    modkey = "Mod4",
  }
  return _M
else -- fallback option
  local _M = {
    terminal = "alacritty",
    term2 = "alacritty -e fish",
    browser = "firefox",
    launcher = "rofi -show drun -sidebar-mode",
    bookmarks = "rofi -show bookmarks -sidebar-mode",
    windowswitcher = "rofi -show window -sidebar-mode",
    screenlock = "/home/alan/.config/awesome/lock.sh",
    editor = "emacsclient -c -a 'emacs'",
    filemanager = "nemo",
    browser2 = "flatpak run com.brave.Browser",
    device = "", -- valid options: desktop | laptop
    modkey = "Mod4",
  }
  return _M
end
