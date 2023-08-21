-- Standard awesome library
local awful      = require("awful")
local xresources = require("beautiful.xresources")
local dpi        = xresources.apply_dpi

local _M         = {}

local lain       = require("lain")

local theme      = require("theme")
local iconsdir   = os.getenv("HOME") .. "/.config/awesome/assets/icons/"

local screen2    = RC.vars.screen2_maybe

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--

function _M.get()
  -- local tags = {}
  -- local tagpairs = {
  --   names = { "1:  ", "2:  ", "3:  ", "4:  ", "5:  ", "6:  ", "7:  ", "8:  ", "9:  ", },
  --   --  
  --   layout = {
  --     RC.layouts[1], RC.layouts[1], RC.layouts[1], RC.layouts[1], RC.layouts[1], RC.layouts[2], RC.layouts[1],
  --     RC.layouts[1], RC.layouts[1],
  --   },
  --   master_width_factor = 0.6,
  -- }
  -- awful.screen.connect_for_each_screen(function(s)
  --   tags[s] = awful.tag(tagpairs.names, s, tagpairs.layout)
  -- end)
  -- return tags
  local tags = {}
  local screen1_tags = {
    awful.tag.add("1:  ", {
      -- icon               = "",
      index              = 1,
      layout             = awful.layout.suit.tile,
      -- layouts = [] -- table of layouts available
      master_fill_policy = theme.master_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      -- screen             = s,
      selected           = true,
    }),

    awful.tag.add("2:  ", {
      -- icon               = "",
      index              = 2,
      layout             = awful.layout.suit.tile,
      -- layouts = [] -- table of layouts available
      master_fill_policy = theme.master_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      -- screen             = s,
    }),

    awful.tag.add("3:  ", {
      -- icon               = "",
      index              = 3,
      layout             = awful.layout.suit.tile,
      -- layouts = [] -- table of layouts available
      master_fill_policy = theme.master_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      -- screen             = s,
    }),

    awful.tag.add("4:  ", {
      -- icon               = "",
      index              = 4,
      layout             = awful.layout.suit.tile,
      -- layouts = [] -- table of layouts available
      master_fill_policy = theme.master_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      -- screen             = s,
    }),

    awful.tag.add("5:  ", {
      -- icon               = "",
      index              = 5,
      layout             = awful.layout.suit.magnifier,
      -- layouts = [] -- table of layouts available
      master_fill_policy = 0.8,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      -- screen             = s,
    }),

    awful.tag.add("6:  ", {
      -- icon               = "",
      index              = 6,
      layout             = lain.layout.termfair.center,
      -- layouts = [] -- table of layouts available
      master_fill_policy = theme.master_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = dpi(25),
      column_count       = theme.column_count,
      -- screen             = s,
    }),

    awful.tag.add("7:  ", {
      -- icon               = "",
      index              = 7,
      layout             = awful.layout.suit.tile,
      -- layouts = [] -- table of layouts available
      master_fill_policy = theme.master_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      -- screen             = s,
    }),

    awful.tag.add("8:  ", {
      -- icon               = "",
      index              = 8,
      layout             = awful.layout.suit.tile,
      -- layouts = [] -- table of layouts available
      master_fill_policy = theme.master_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      -- screen             = s,
    }),

    awful.tag.add("9:  ", {
      -- icon               = "",
      index              = 9,
      layout             = awful.layout.suit.tile,
      -- layouts = [] -- table of layouts available
      master_fill_policy = theme.master_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      -- screen             = s,
    }),
  }

  local mon2_fill_policy = theme.master_fill_policy -- "master_width_factor"

  -- tags for the second screen:
  local screen2_tags = {
    awful.tag.add("1:  ", {
      -- icon               = "",
      index              = 1,
      layout             = awful.layout.suit.tile.bottom,
      -- layouts = [] -- table of layouts available
      master_fill_policy = mon2_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = screen2,
      selected           = true,
    }),

    awful.tag.add("2:  ", {
      -- icon               = "",
      index              = 2,
      layout             = awful.layout.suit.tile.bottom,
      -- layouts = [] -- table of layouts available
      master_fill_policy = mon2_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = screen2,
    }),

    awful.tag.add("3:  ", {
      -- icon               = "",
      index              = 3,
      layout             = awful.layout.suit.tile.bottom,
      -- layouts = [] -- table of layouts available
      master_fill_policy = mon2_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = screen2,
    }),

    awful.tag.add("4:  ", {
      -- icon               = "",
      index              = 4,
      layout             = awful.layout.suit.tile.bottom,
      -- layouts = [] -- table of layouts available
      master_fill_policy = mon2_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = screen2,
    }),

    awful.tag.add("5:  ", {
      -- icon               = "",
      index              = 5,
      layout             = awful.layout.suit.tile.bottom,
      -- layouts = [] -- table of layouts available
      master_fill_policy = mon2_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = screen2,
    }),

    awful.tag.add("6:  ", {
      -- icon               = "",
      index              = 6,
      layout             = awful.layout.suit.tile.bottom,
      -- layouts = [] -- table of layouts available
      master_fill_policy = mon2_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = screen2,
    }),

    awful.tag.add("7:  ", {
      -- icon               = "",
      index              = 7,
      layout             = awful.layout.suit.tile.bottom,
      -- layouts = [] -- table of layouts available
      master_fill_policy = mon2_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = screen2,
    }),

    awful.tag.add("8:  ", {
      -- icon               = "",
      index              = 8,
      layout             = awful.layout.suit.tile.bottom,
      -- layouts = [] -- table of layouts available
      master_fill_policy = mon2_fill_policy,
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = screen2,
    }),
  }
  -- set up second monitor:
  if screen.count == 1 then
    my_tags = screen1_tags
  else
    my_tags = { unpack(screen1_tags) }
    for I = 1, #screen2_tags do
      my_tags[#screen1_tags + I] = screen2_tags[I]
    end
  end

  awful.screen.connect_for_each_screen(function(s)
    tags[s] = my_tags
  end)
  return tags
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

---@diagnostic disable-next-line: redundant-parameter
return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
