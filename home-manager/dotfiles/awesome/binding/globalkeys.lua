-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
-- local hotkeys_popup = require("awful.hotkeys_popup").widget
local hotkeys_popup = require("awful.hotkeys_popup")

-- Resource Configuration
local modkey = RC.vars.modkey
local terminal = RC.vars.terminal
local term2 = RC.vars.term2
local browser = RC.vars.browser
local browser2 = RC.vars.browser2
local launcher = RC.vars.launcher
local editor = RC.vars.editor
local filemanager = RC.vars.filemanager
local screenlock = RC.vars.screenlock
local bookmarks = RC.vars.bookmarks
local windowswitcher = RC.vars.windowswitcher
local powerMenu = RC.vars.powerMenu
local screenshot = RC.vars.screenshot
local workapps = RC.vars.start_work_apps

local lain = require("lain")

local _M = {}

-- reading
-- https://awesomewm.org/wiki/Global_Keybindings

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- cycle through nonempty tags only
-- credit to https://www.reddit.com/r/awesomewm/comments/lzly7b/comment/gqcocl3/?utm_source=share&utm_medium=web2x&context=3
-- TODO how to cycle backwards?
local function tag_view_nonempty(step, s)
  step = step or 1
  s = s or awful.screen.focused()
  local tags = s.tags
  local bound = step > 0 and #tags or 1

  for i = s.selected_tag.index + step, bound, step do
    local t = tags[i]
    if #t:clients() > 0 then
      t:view_only()
      return
    end
  end

  for i = step == 1 and 1 or 9, bound, step do
    local t = tags[i]
    if #t:clients() > 0 then
      t:view_only()
      return
    end
  end
end

function _M.get()
  local globalkeys = gears.table.join(
    awful.key({ modkey, }, "s", hotkeys_popup.show_help,
      { description = "show help", group = "awesome" }),

    -- Tag browsing
    awful.key({ modkey, }, "Tab", tag_view_nonempty,
      { description = "view next", group = "tag" }),
    awful.key({ modkey, "Shift" }, "Tab", awful.tag.viewprev,
      { description = "view previous", group = "tag" }),
    awful.key({ modkey }, "w", function() awful.spawn(windowswitcher) end,
      { description = "list windows", group = "launcher" }),

    -- show/hide the top bar
    awful.key({ modkey, }, "b", function()
        awful.screen.focused().mywibox.visible = not awful.screen.focused().mywibox.visible
      end,
      { description = "show/hide top bar", group = "awesome" }
    ),

    -- resize gaps on the fly
    awful.key({ modkey, }, "-", function() lain.util.useless_gaps_resize(1) end),
    awful.key({ modkey, }, "=", function() lain.util.useless_gaps_resize(-1) end),

    awful.key({ modkey }, "Down",
      function()
        awful.client.focus.bydirection("down")
        if client.focus then client.focus:raise() end
      end),
    awful.key({ modkey }, "Up",
      function()
        awful.client.focus.bydirection("up")
        if client.focus then client.focus:raise() end
      end),
    awful.key({ modkey }, "Left",
      function()
        awful.client.focus.bydirection("left")
        if client.focus then client.focus:raise() end
      end),
    awful.key({ modkey }, "Right",
      function()
        awful.client.focus.bydirection("right")
        if client.focus then client.focus:raise() end
      end),

    awful.key({ modkey, }, "j",
      function()
        awful.client.focus.byidx(1)
      end,
      { description = "focus next by index", group = "client" }
    ),
    awful.key({ modkey, }, "k",
      function()
        awful.client.focus.byidx(-1)
      end,
      { description = "focus previous by index", group = "client" }
    ),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Layout manipulation
    awful.key({ modkey, "Shift" }, "j", function() awful.client.swap.byidx(1) end,
      { description = "swap with next client by index", group = "client" }),
    awful.key({ modkey, "Shift" }, "k", function() awful.client.swap.byidx(-1) end,
      { description = "swap with previous client by index", group = "client" }),
    awful.key({ modkey, "Control" }, "j", function() awful.screen.focus_relative(1) end,
      { description = "focus the next screen", group = "screen" }),
    awful.key({ modkey, "Control" }, "k", function() awful.screen.focus_relative(-1) end,
      { description = "focus the previous screen", group = "screen" }),

    -- Standard program
    awful.key({ modkey, }, "Return", function() awful.spawn(terminal) end,
      { description = "open a terminal", group = "launcher" }),
    awful.key({ modkey, "Shift" }, "Return", function() awful.spawn(term2) end,
      { description = "term without tmux", group = "launcher" }),

    -- open all of my work programs at once:
    awful.key({ modkey, "Control" }, "Return", workapps,
      { description = "launch all work apps", group = "launcher" }),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
      { description = "reload awesome", group = "awesome" }),
    awful.key({ modkey, "Shift" }, "q", awesome.quit,
      { description = "quit awesome", group = "awesome" }),
    awful.key({ modkey, }, "`", function() awful.spawn(screenlock) end,
      { description = "lock screen", group = "awesome" }),
    awful.key({ modkey, }, "g", function() awful.spawn(bookmarks) end,
      { description = "browse bookmarks", group = "launcher" }),

    -- Show/hide the systray:
    awful.key({ modkey }, "/", function()
        awful.screen.focused().systray.visible = not awful.screen.focused().systray.visible
      end,
      { description = "toggle systray visibility", group = "awesome" }
    ),


    -- Audio control:
    awful.key({ modkey }, "F10", function()
        awful.spawn.with_shell("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
      end,
      { description = "toggle volume mute", group = "audio" }
    ),

    awful.key({ modkey }, "F11", function()
        awful.spawn.with_shell("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
      end,
      { description = "volume down 5%", group = "audio" }
    ),

    awful.key({ modkey }, "F12", function()
        awful.spawn.with_shell("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
      end,
      { description = "volume up 5%", group = "audio" }
    ),

    awful.key({ modkey, }, ",",
      function() awful.spawn("clipmenu") end,
      { description = "clipboard history", group = "launcher" }
    ),
    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Layout manipulation
    awful.key({ modkey, }, "l", function() awful.tag.incmwfact(0.05) end,
      { description = "increase master width factor", group = "layout" }),
    awful.key({ modkey, }, "h", function() awful.tag.incmwfact(-0.05) end,
      { description = "decrease master width factor", group = "layout" }),
    awful.key({ modkey, "Shift" }, "h", function() awful.tag.incnmaster(1, nil, true) end,
      { description = "increase the number of master clients", group = "layout" }),
    awful.key({ modkey, "Shift" }, "l", function() awful.tag.incnmaster(-1, nil, true) end,
      { description = "decrease the number of master clients", group = "layout" }),
    awful.key({ modkey, "Control" }, "h", function() awful.tag.incncol(1, nil, true) end,
      { description = "increase the number of columns", group = "layout" }),
    awful.key({ modkey, "Control" }, "l", function() awful.tag.incncol(-1, nil, true) end,
      { description = "decrease the number of columns", group = "layout" }),
    awful.key({ modkey, }, "space", function() awful.layout.inc(1) end,
      { description = "select next", group = "layout" }),
    awful.key({ modkey, "Shift" }, "space", function() awful.layout.inc(-1) end,
      { description = "select previous", group = "layout" }),

    awful.key({ modkey, "Control" }, "n",
      function()
        local c = awful.client.restore()
        -- Focus restored client
        if c then
          c:emit_signal(
            "request::activate", "key.unminimize", { raise = true }
          )
        end
      end,
      { description = "restore minimized", group = "client" }),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    awful.key({ modkey }, "d", function() awful.spawn(launcher) end,
      { description = "launcher", group = "launcher" }
    ),

    awful.key({ modkey }, "x", function() awful.spawn(browser) end,
      { description = "browser", group = "applications" }
    ),

    awful.key({ modkey, "Shift" }, "x", function() awful.spawn(browser2) end,
      { description = "brave browser", group = "applications" }
    ),

    awful.key({ modkey }, "z", function() awful.spawn(filemanager) end,
      { description = "file manager", group = "applications" }
    ),

    awful.key({ modkey }, "c", function() awful.spawn("discord") end,
      { description = "discord", group = "applications" }
    ),


    awful.key({ modkey }, "v", function() awful.spawn(editor) end,
      { description = "text editor", group = "applications" }
    ),
    awful.key({ modkey }, "Escape", function() awful.spawn(powerMenu) end,
      { description = "power menu", group = "launcher" }
    ),

    awful.key({ modkey }, "p", function() awful.spawn(screenshot) end,
      { description = "screenshot", group = "launcher" }
    ),



    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    awful.key({ modkey, "Control" }, "Up",
      function() awful.client.moveresize(0, 0, 0, -20) end),
    awful.key({ modkey, "Control" }, "Down",
      function() awful.client.moveresize(0, 0, 0, 20) end),
    awful.key({ modkey, "Control" }, "Left",
      function() awful.client.moveresize(0, 0, -20, 0) end),
    awful.key({ modkey, "Control" }, "Right",
      function() awful.client.moveresize(0, 0, 20, 0) end),

    -- Move
    awful.key({ modkey, "Shift" }, "Down",
      function() awful.client.moveresize(0, 20, 0, 0) end),
    awful.key({ modkey, "Shift" }, "Up",
      function() awful.client.moveresize(0, -20, 0, 0) end),
    awful.key({ modkey, "Shift" }, "Left",
      function() awful.client.moveresize(-20, 0, 0, 0) end),
    awful.key({ modkey, "Shift" }, "Right",
      function() awful.client.moveresize(20, 0, 0, 0) end)

  )

  return globalkeys
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

---@diagnostic disable-next-line: redundant-parameter
return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
