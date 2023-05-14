-- Standard awesome library
local awful = require("awful")

local _M = {}

local device = RC.vars.device

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

if device == "desktop" then
  function _M.get()
    local tags = {}
    local tagpairs = {
      -- awful.screen.connect_for_each_screen(function(s)
      -- Each screen has its own tag table.
      names = { "1:  ", "2:  ", "3:  ", "4:  ", "5:  ", "6:  ", "7:  ", "8:  ", "9:  ", }, --
      -- 
      layout = {
        RC.layouts[1], RC.layouts[1], RC.layouts[2], RC.layouts[2], RC.layouts[1], RC.layouts[2], RC.layouts[1],
        RC.layouts[1], RC.layouts[1],
      }
    }

    awful.screen.connect_for_each_screen(function(s)
      tags[s] = awful.tag(tagpairs.names, s, tagpairs.layout)
    end)

    return tags
  end
elseif device == "laptop" then
  function _M.get()
    local tags = {}
    local tagpairs = {
      -- awful.screen.connect_for_each_screen(function(s)
      -- Each screen has its own tag table.
      names = { "1:  ", "2:  ", "3:  ", "4:  ", "5:  ", "6:  ", "7:  ", "8:  " },
      layout = {
        RC.layouts[1], RC.layouts[1], RC.layouts[1], RC.layouts[1], RC.layouts[2], RC.layouts[2], RC.layouts[1],
        RC.layouts[1]
      }
    }
    awful.screen.connect_for_each_screen(function(s)
      tags[s] = awful.tag(tagpairs.names, s, tagpairs.layout)
    end)

    return tags
  end
else -- fallback option
  function _M.get()
    local tags = {}
    local tagpairs = {
      -- awful.screen.connect_for_each_screen(function(s)
      -- Each screen has its own tag table.
      names = { " F ", " A ", " L ", " L ", " B ", " A ", " C ", " K " }, -- tag[s] = awful.tag(names, s, RC.layouts[2])
      layout = {
        RC.layouts[1], RC.layouts[1], RC.layouts[1], RC.layouts[1], RC.layouts[1], RC.layouts[1], RC.layouts[1],
        RC.layouts[1]
      }
    }
    awful.screen.connect_for_each_screen(function(s)
      tags[s] = awful.tag(tagpairs.names, s, tagpairs.layout)
    end)

    return tags
  end
end
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
