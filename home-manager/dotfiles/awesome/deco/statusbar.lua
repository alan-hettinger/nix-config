-- Standard awesome library
local gears            = require("gears")
local awful            = require("awful")

-- Wibox handling library
local wibox            = require("wibox")

-- Custom Local Library: Common Functional Decoration
local deco             = {
  wallpaper = require("deco.wallpaper"),
  taglist   = require("deco.taglist"),
  tasklist  = require("deco.tasklist")
}

local taglist_buttons  = deco.taglist()
local tasklist_buttons = deco.tasklist()

local _M               = {}
local theme            = require("theme")
local screenlock       = RC.vars.screenlock
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- add libraries for third party widgets
local mpris_widget     = require("awesome-wm-widgets.mpris-widget")
local lain             = require("lain")
local logout_menu      = require("awesome-wm-widgets.logout-menu-widget.logout-menu")

local musicbox         = wibox.widget { {
  {
    {
      mpris_widget(),
      fg = "#b7bdf8",
      font = theme.taglist_font,
      widget = wibox.container.background,
    },
    left = 10,
    right = 10,
    top = 3,
    bottom = 3,
    widget = wibox.container.margin
  },
  shape = gears.shape.rounded_rect,
  bg = theme.bg_normal,
  shape_border_color = theme.bg_focus,
  shape_border_width = 2,
  widget = wibox.container.background
},
  layout = wibox.layout.align.horizontal,
}

local logout           = logout_menu {
  onlock = function() awful.spawn.with_shell(screenlock) end,
  onreboot = function() awful.spawn.with_shell("systemctl reboot") end,
  onpoweroff = function() awful.spawn.with_shell("systemctl poweroff") end,
}

local temp             = lain.widget.temp {
  -- tempfile = "/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon1/temp1_input",
  -- temp1 is tctl, temp3 is Tccd1
  settings = function()
    timeout = 2
    widget:set_markup("" .. coretemp_now .. "°C")
  end
}

local tempbox          = wibox.widget { {
  {
    {
      temp,
      fg = "#8aadf4",
      font = theme.taglist_font,
      widget = wibox.container.background,
    },
    left = 10,
    right = 10,
    top = 3,
    bottom = 3,
    widget = wibox.container.margin
  },
  shape = gears.shape.rectangle,
  bg = theme.bg_normal,
  shape_border_color = theme.border_normal,
  shape_border_width = 2,
  widget = wibox.container.background
},
  layout = wibox.layout.align.horizontal,
}

local tempgpu          = lain.widget.temp {
  tempfile = ("/sys/devices/pci0000:00/0000:00:03.1/0000:2b:00.0/0000:2c:00.0/0000:2d:00.0/hwmon/hwmon1/temp2_input"), -- junction
  settings = function()
    timeout = 2
    if coretemp_now ~= "N/A" then
      widget:set_markup("  GPU: " .. coretemp_now .. "°C  ")
    else
      widget:set_markup("")
    end
  end
}


local gputempbox = wibox.widget { {
  {
    {
      tempgpu,
      fg = "#ee99a0",
      font = theme.taglist_font,
      widget = wibox.container.background,
    },
    -- left = 10,
    -- right = 10,
    -- top = 3,
    -- bottom = 3,
    widget = wibox.container.margin
  },
  shape = gears.shape.rectangle,
  bg = theme.bg_normal,
  shape_border_color = theme.border_normal,
  shape_border_width = 2,
  widget = wibox.container.background
},
  layout = wibox.layout.align.horizontal,
}

local mem = lain.widget.mem {
  settings = function()
    widget:set_markup("RAM: " .. mem_now.perc .. "%")
  end
}

local membox = wibox.widget { {
  {
    {
      mem,
      fg = "#eed49f",
      widget = wibox.container.background,
      font = theme.taglist_font,
    },
    left = 10,
    right = 10,
    top = 3,
    bottom = 3,
    widget = wibox.container.margin
  },
  shape = gears.shape.rectangle,
  bg = theme.bg_normal,
  shape_border_color = theme.border_normal,
  shape_border_width = 2,
  widget = wibox.container.background
},
  layout = wibox.layout.align.horizontal,
}

local cpu = lain.widget.cpu {
  settings = function()
    widget:set_markup("CPU: " .. cpu_now.usage .. "%")
  end
}

local cpubox = wibox.widget { {
  {
    {
      cpu,
      fg = "#8aadf4",
      widget = wibox.container.background,
    },
    left = 10,
    right = 10,
    top = 3,
    bottom = 3,
    widget = wibox.container.margin
  },
  shape = gears.shape.rectangle,
  bg = theme.bg_normal,
  shape_border_color = theme.border_normal,
  shape_border_width = 2,
  widget = wibox.container.background
},
  layout = wibox.layout.align.horizontal,
}

local bat = lain.widget.bat {
  notify = "off",
  timeout = 10,
  settings = function()
    if bat_now.status and bat_now.status ~= "N/A" then
      if bat_now.ac_status == 1 then
        widget:set_markup("")
      else
        widget:set_markup("  : " .. bat_now.perc .. "%, " .. bat_now.time .. "  ")
      end
    else
      widget:set_markup("")
    end
  end
}
local batbox = wibox.widget { {
  {
    {
      bat,
      fg = "#f4dbd6",
      widget = wibox.container.background,
    },
    -- left = 10,
    -- right = 10,
    -- top = 3,
    -- bottom = 3,
    widget = wibox.container.margin
  },
  shape = gears.shape.rectangle,
  bg = theme.bg_normal,
  shape_border_color = theme.border_normal,
  shape_border_width = 2,
  widget = wibox.container.background
},
  layout = wibox.layout.align.horizontal,
}

-- Create a textclock widget
local mytextclock = wibox.widget {
  format = '%a %b %d, %I:%M %P',
  widget = wibox.widget.textclock,
  margins = 5,
}

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper
  set_wallpaper(s)

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
    awful.button({}, 1, function() awful.layout.inc(1) end),
    awful.button({}, 3, function() awful.layout.inc(-1) end),
    awful.button({}, 4, function() awful.layout.inc(1) end),
    awful.button({}, 5, function() awful.layout.inc(-1) end)
  ))

  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = taglist_buttons,
    style   = {
      shape = gears.shape.rounded_rect
    },
    layout  = {
      spacing = 6,
      spacing_widget = {
        color = theme.bg_normal,
        widget = wibox.widget.separator
      },
      layout = wibox.layout.fixed.horizontal,
    },
  }

  s.systray = wibox.widget.systray()
  s.systray.visible = true

  s.mytasklist = awful.widget.tasklist {
    screen = s,
    filter = awful.widget.tasklist.filter.alltags,
    buttons = tasklist_buttons,
    style = {
      shape = gears.shape.rounded_rect,
    },
  }

  local layoutwrapper = wibox.widget { {
    {
      {
        s.mylayoutbox,
        fg = theme.fg_normal,
        widget = wibox.container.background,
      },
      left = 5,
      right = 10,
      top = 3,
      bottom = 3,
      widget = wibox.container.margin
    },
    shape = gears.shape.rounded_rect,
    -- bg = "#1e1e2e",
    -- shape_border_color = theme.border_normal,
    -- shape_border_width = 3,
    widget = wibox.container.background
  },
    layout = wibox.layout.align.horizontal,
  }


  -- Create the wibox
  s.mywibox = awful.wibar({
    position = "top",
    screen = s,
    visible = true,
    bg = theme.bg_normal,
    border_width = 2,
    border_color = theme.border_normal,
    ontop = false,
    -- height = 40,
  })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = {
      spacing = 6,
      spacing_widget = {
        color = "#1e1e2e",
        widget = wibox.widget.separator
      },
    },
    layout = wibox.layout.align.horizontal,
    expand = "none",
    {
      -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      -- s.mylayoutbox,
      layoutwrapper,
      s.mytaglist,
    },
    {
      -- middle widgets:
      layout = wibox.layout.fixed.horizontal,
      mytextclock, -- Middle widget
    },
    {
      -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      batbox,
      membox,
      gputempbox,
      cpubox,
      tempbox,
      -- musicbox,
      s.systray,
      logout,
    },
  }
end)
