local awful = require("awful")

-- returns a screen object. If there is only 1 screen, returns it, otherwise returns screen to the right
local function screen2_if_exists()
  if screen.count == 1 then
    return awful.screen.focused()
  else
    -- awful.screen.getbycoord() returns the index of a screen, but we want the actual object, so we wrap it
    -- TODO: make this work for laptop. Currently hardcoded for my desktop setup.
    return screen[awful.screen.getbycoord(2560, 0)]
  end
end

local emacs_command = "emacsclient -c -e '(+eshell/here)'"

local _M = {
  -- terminal = "wezterm connect unix",
  -- terminal = emacs_command,
  terminal = "alacritty",
  term2 = "wezterm",
  browser = "firefox",
  launcher = [[
               rofi -show combi \
                    -location 1 \
                    -display-combi '>>' \
                    -theme-str '#window { y-offset: 45px; }'
             ]],
  bookmarks = "rofi -show bookmarks -sidebar-mode",
  windowswitcher = "rofi -show window -sidebar-mode",
  powerMenu = [[rofi -show power-menu \
                     -modi power-menu:rofi-power-menu \
                     -location 3 \
                     -theme-str '#window { y-offset: 45px; width: 375px; height: 325px; }']],
  screenlock = "xflock4",
  screenshot = "flameshot gui",
  editor = emacs_command,
  filemanager = "dolphin --platformtheme gtk --style kvantum", -- FIXME dumb as hell workaround
  -- filemanager = "nemo", -- nemo | thunar | dolphin
  browser2 = "brave",
  device = "desktop", -- valid options: desktop | laptop
  modkey = "Mod4",

  -- for multi-monitor purposes:
  screen1 = screen.primary,
  screen2_maybe = screen2_if_exists(),

  -- to be spawned all at once with a keybind:
  start_work_apps = function()
    awful.spawn("firefox")
    awful.spawn("zotero")
    awful.spawn("discord")
    awful.spawn("thunderbird")
    awful.spawn("emacsclient -c '~/Documents/Notes/working-todo.org'", { tag = awful.screen.focused().tags[2] })
  end,

}
return _M
