-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Custom Local Library: Common Functional Decoration
require("deco.titlebar")

-- reading
-- https://awesomewm.org/apidoc/classes/signals.html

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

-- client.connect_signal("manage", function(c)
--     if c.floating or c.first_tag.layout.name == "floating" then
--         awful.titlebar.show(c)
--     else
--         awful.titlebar.hide(c)
--     end
-- end)
-- tag.connect_signal("property::layout", function(t)
--   local clients = t:clients()
--   for k, c in pairs(clients) do
--     if c.floating or c.first_tag.layout.name == "floating" then
--       awful.titlebar.show(c)
--     else
--       awful.titlebar.hide(c)
--     end
--   end
-- end)
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
-- client.connect_signal("manage",
--                       function(c)
--                         c.maximized = false
--     end)

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
--     c:emit_signal("request::activate", "mouse_enter", {raise = false})
-- end)
--

-- local function set_border(c)
--     local s = awful.screen.focused().selected_tag
--     if c.maximized
--         or (#s:clients() == 1)
--     then
--         c.border_width = 0
--     else
--         c.border_width = beautiful.border_width
--     end
-- end

screen.connect_signal("arrange", function(s)
  -- local only_one = #s.tiled_clients == 1 -- use tiled_clients so that other floating windows don't affect the count
  -- but iterate over clients instead of tiled_clients as tiled_clients doesn't include maximized windows
  for _, c in pairs(s.clients) do
    -- if c.first_tag.layout.name == "floating" then
    --   c.border_width = beautiful.border_width
    -- elseif (only_one) and not c.floating or c.maximized then
    --   c.border_width = 0
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
