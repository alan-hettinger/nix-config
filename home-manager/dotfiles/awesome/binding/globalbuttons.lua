-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local globalbuttons = gears.table.join(
  -- awful.button({ }, 3, function () RC.mainmenu:toggle() end) -- I don't like the behavior of right clicking on desktop for menu
  -- -- scrolling on desktop switches tags - I don't actually like this behavior
  -- awful.button({ }, 5, awful.tag.viewnext),
  -- awful.button({ }, 4, awful.tag.viewprev)
  )

  return globalbuttons
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

---@diagnostic disable-next-line: redundant-parameter
return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
