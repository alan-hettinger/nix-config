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
    awful.layout.suit.fair,      -- 3
    awful.layout.suit.magnifier, -- 4
--     bling.layout.mstab,          -- 5
    -- awful.layout.suit.floating,
    -- bling.layout.centered,
    -- bling.layout.vertical,
    -- bling.layout.horizontal,
    -- bling.layout.equalarea,
    -- bling.layout.deck,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.corner.nw,
  }

  return layouts
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
