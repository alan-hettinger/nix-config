local awful = require("awful")

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
local device = RC.vars.device

local function run_once(cmd)
  local findme = cmd
  local firstspace = cmd:find(' ')
  if firstspace then findme = cmd:sub(0, firstspace - 1) end
  awful.spawn.with_shell(string.format(
    'pgrep -u $USER -x %s > /dev/null || (%s)',
    findme, cmd), false)
end

run_once("dbus-update-activation-environment --all")
-- run_once("/usr/libexec/polkit-gnome-authentication-agent-1")
-- run_once("/home/alan/.screenlayout/layout.sh")
-- run_once("xfce4-power-manager")                                                 -- Power Management
-- run_once("unclutter --timeout 30 --jitter 5 --exclude-root --ignore-scrolling") --
-- run_once("setxkbmap -option caps:escape &")
-- run_once("nm-applet")
-- run_once("nextcloud")

if device == "desktop" then
  run_once("/home/alan/x-autorun.sh") -- script currently isn't set up for laptop
  run_once("picom")                   -- Compositor
end

if device == "laptop" then
  run_once("pasystray")                          -- PulseAudio, currently using as GUI tool for pipewire
  run_once("/home/alan/.screenlayout/layout.sh") -- script not setup for desktop
  run_once("picom")                              -- Compositor
end
