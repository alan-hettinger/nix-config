-- Standard awesome library
local awful = require("awful")

local _M = {}

local lain = require("lain")

-- local bling = require("bling")
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local layouts = {
    awful.layout.suit.tile,      -- 1
    lain.layout.termfair.center, -- 2
    -- centerfair,
    awful.layout.suit.fair,      -- 3
    awful.layout.suit.magnifier, -- 4
  }

  return layouts
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
