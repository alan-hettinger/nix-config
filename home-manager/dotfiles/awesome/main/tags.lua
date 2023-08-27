-- Standard awesome library
local awful      = require("awful")
local xresources = require("beautiful.xresources")
local dpi        = xresources.apply_dpi

local _M         = {}

local lain       = require("lain")

local theme      = require("theme")
local iconsdir   = os.getenv("HOME") .. "/.config/awesome/assets/icons/"
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local tags = {}

  local screen1 = {
    tag_names = { "1:  ", "2:  ", "3:  ", "4:  ", "5:  ", "6:  ", "7:  ", "8:  ", "9:  " },
    defaults = {
      layout             = awful.layout.suit.tile,
      master_fill_policy = "expand", -- "master_width_factor"|"expand"
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = RC.vars.screen1,
    },
    default_selected_tag = 1,
    overrides = {
      -- Overrides should always be of the form { tag index, property, value }
      { 5, "layout",             awful.layout.suit.magnifier },
      { 5, "master_fill_policy", 0.8 },
      { 6, "layout",             lain.layout.termfair.center },
      { 6, "gap",                dpi(25) },
    }
  }

  local screen2 = {
    tag_names = { "1:  ", "2:  ", "3:  ", "4:  ", "5:  ", "6:  ", "7:  ", "8:  " },
    defaults = {
      layout             = awful.layout.suit.tile.bottom,
      master_fill_policy = "expand", -- "master_width_factor"|"expand"
      master_count       = theme.master_count,
      gap_single_client  = theme.gap_single_client,
      gap                = theme.useless_gap,
      column_count       = theme.column_count,
      screen             = RC.vars.screen2_maybe,
    },
    default_selected_tag = 6,
  }

  local setup_per_screen = function(target_screen)
    local make_tag_table = function(tag_names, target_table)
      for I = 1, #tag_names do
        target_table[I] = awful.tag.add(tag_names[I], { index = I })
      end
    end

    local set_defaults = function()
      local props = { 'layout', 'master_fill_policy', 'master_count', 'gap_single_client', 'gap', 'column_count',
        'screen' }
      for I = 1, #target_screen.tags do
        local thistag = target_screen.tags[I]
        for v = 1, #props do
          local thisprop = props[v]
          -- if thistag[v] == nil then
          thistag[thisprop] = target_screen.defaults[thisprop]
          -- end
        end
      end
      target_screen.tags[target_screen.default_selected_tag].selected = true
    end


    local override = function(tag, prop, val)
      target_screen.tags[tag][prop] = val
    end
    local set_overrides = function()
      if target_screen.overrides ~= nil then
        for I = 1, #target_screen.overrides do
          local tag = target_screen.overrides[I][1]
          local prop = target_screen.overrides[I][2]
          local val = target_screen.overrides[I][3]
          override(tag, prop, val)
        end
      end
    end

    target_screen.tags = {}
    make_tag_table(target_screen.tag_names, target_screen.tags)
    set_defaults()
    set_overrides()
  end

  setup_per_screen(screen1)
  setup_per_screen(screen2)

  -- set up second monitor:
  local my_tags = {}
  if screen.count == 1 then
    my_tags = screen1.tags
  else
    my_tags = { table.unpack(screen1.tags) }
    for I = 1, #screen2.tags do
      my_tags[#screen1.tags + I] = screen2.tags[I]
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
