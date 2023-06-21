-- Standard awesome library
local gears           = require("gears")
local awful           = require("awful")

-- Wibox handling library
local wibox           = require("wibox")

-- Custom Local Library: Common Functional Decoration
local deco            = {
  taglist = require("deco.taglist"),
}

local taglist_buttons = deco.taglist()

local _M              = {}
local theme           = require("theme")
local screenlock      = RC.vars.screenlock
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- add libraries for third party widgets
local lain            = require("lain")

local temp            = lain.widget.temp {
  -- tempfile = "/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon1/temp1_input",
  -- temp1 is tctl, temp3 is Tccd1
  settings = function()
    timeout = 2
    widget:set_markup("" .. coretemp_now .. "°C")
  end
}

local tempbox         = wibox.widget { {
  {
    {
      temp,
      fg = theme.blue,
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

local tempgpu         = lain.widget.temp {                                                                             -- TODO find a way to make this more portable
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
      fg = theme.lightRed,
      font = theme.taglist_font,
      widget = wibox.container.background,
    },
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
      fg = theme.yellow,
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
      fg = theme.blue,
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
      fg = theme.lightYellow,
      widget = wibox.container.background,
    },
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
        color = theme.bg_normal,
        widget = wibox.widget.separator
      },
    },
    layout = wibox.layout.align.horizontal,
    expand = "none",
    {
      -- Left widgets
      layout = wibox.layout.fixed.horizontal,
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
      s.systray,
    },
  }
end)
