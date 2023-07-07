-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Custom Local Library: Common Functional Decoration
require("deco.titlebar")

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function(c)
  -- Set the windows at the slave,
  -- i.e. put it at the end of others instead of setting it master.
  if not awesome.startup then awful.client.setslave(c) end

  if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
    -- Prevent clients from being unreachable after screen count changes.
    awful.placement.no_offscreen(c)
  end
end)

-- focus urgent clients (this should only happen when they're pinged by a window switcher)
client.connect_signal("property::urgent", function(c)
  c.minimized = false
  c:jump_to()
end)

client.connect_signal("property::floating", function(c)
  if c.floating then
    awful.titlebar.show(c)
  else
    awful.titlebar.hide(c)
  end
end)

client.connect_signal("property::maximized", function(c)
  if c.maximized then
    awful.titlebar.hide(c)
  end
end)

tag.connect_signal("property::nmaster", function(t)
  t.master_count = 5
end)

client.connect_signal("property::minimized", function(c)
  c.minimized = false
end)

-- I don't know what this does but I added it for a reason at some point
client.connect_signal("property::fullscreen", function(c)
  if c.fullscreen then
    gears.timer.delayed_call(function()
      if c.valid then
        c:geometry(c.screen.geometry)
      end
    end)
  end
end)

screen.connect_signal("arrange", function(s)
  for _, c in pairs(s.clients) do
    if c.maximized or c.fullscreen then
      c.border_width = 0
    else
      c.border_width = beautiful.border_width
    end
  end
end)

-- Restart Awesome when screens change:
screen.connect_signal("removed", awesome.restart)
screen.connect_signal("added", awesome.restart)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
