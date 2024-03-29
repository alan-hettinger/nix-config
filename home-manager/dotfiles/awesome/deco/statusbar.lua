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

local theme           = require("theme")
local iconsdir        = os.getenv("HOME") .. "/.config/awesome/assets/icons/"
local launcher        = RC.vars.launcher
local powerMenu       = RC.vars.powerMenu
local screen1         = RC.vars.screen1
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

-- a widget to display the active client and TODO display various common clickable actions
local active_client_unwrapped = awful.widget.tasklist {
  filter = awful.widget.tasklist.filter.focused,
  screen = screen1,
  style = {
    align = "center",
    bg_focus = theme.bg_normal,
    fg_focus = theme.fg_normal,
  },
}

local active_client = wibox.widget { {
  {
    {
      active_client_unwrapped,
      fg = theme.fg_normal,
      widget = wibox.container.background,
    },
    widget = wibox.container.margin
  },
  shape = gears.shape.rounded_rect,
  bg = theme.bg_normal,
  shape_border_color = theme.border_normal,
  shape_border_width = 2,
  -- forced_width = 800,
  widget = wibox.container.background
},
  layout = wibox.layout.align.horizontal,
}

-- Create a textclock widget
local mytextclock = wibox.widget {
  format = '  %a %b %d, %I:%M %P  ',
  widget = wibox.widget.textclock,
  margins = 5,
}

local clockbox = wibox.widget { {
  {
    {
      mytextclock,
      fg = theme.fg_normal,
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

-- widget to launch the preferred launcher application (eg rofi)
local my_launcher = wibox.widget {
  image = iconsdir .. "home.svg",
  resize = true,
  widget = wibox.widget.imagebox,
}
my_launcher:buttons(gears.table.join(
  awful.button({}, 1, function() awful.spawn(launcher) end)
))

local logout_button = wibox.widget {
  image = iconsdir .. "log-out.svg",
  resize = true,
  widget = wibox.widget.imagebox,
}
logout_button:buttons(gears.table.join(
  awful.button({}, 1, function() awful.spawn(powerMenu) end)
))

-- draw different wibars per screen
local function wibox_primary(s)
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
      widget_template = {
        {
          {
            id = 'icon_role',
            widget = wibox.widget.imagebox,
          },
          margins = 4,
          widget = wibox.container.margin,
        },
        {
          {
            id = 'index_role',
            widget = wibox.widget.textbox,
          },
          margins = 2,
          widget = wibox.container.margin,
        },
      },
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
    shape_border_color = theme.border_normal,
    shape_border_width = 2,
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
    ---@diagnostic disable-next-line: duplicate-index
    layout = {
      spacing = 6,
      spacing_widget = {
        color = theme.bg_normal,
        widget = wibox.widget.separator
      },
    },
    ---@diagnostic disable-next-line: duplicate-index
    layout = wibox.layout.align.horizontal,
    expand = "inside", -- inside|outside|none
    {
      -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      my_launcher,
      layoutwrapper,
      s.mytaglist,
    },
    {
      -- middle widgets:
      layout = wibox.layout.fixed.horizontal,
      active_client,
      -- mytextclock, -- Middle widget
    },
    {
      -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      -- mytextclock,
      s.systray,
      batbox,
      membox,
      gputempbox,
      cpubox,
      tempbox,
      clockbox,
      logout_button,
    },
  }
end

local function wibox_secondary(s)
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
      widget_template = {
        {
          {
            id = 'icon_role',
            widget = wibox.widget.imagebox,
          },
          margins = 4,
          widget = wibox.container.margin,
        },
        {
          {
            id = 'index_role',
            widget = wibox.widget.textbox,
          },
          margins = 2,
          widget = wibox.container.margin,
        },
      },
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
    ---@diagnostic disable-next-line: duplicate-index
    layout = {
      spacing = 6,
      spacing_widget = {
        color = theme.bg_normal,
        widget = wibox.widget.separator
      },
    },
    ---@diagnostic disable-next-line: duplicate-index
    layout = wibox.layout.align.horizontal,
    expand = "none",
    {
      -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      layoutwrapper,
    },
    {
      -- middle widgets:
      layout = wibox.layout.fixed.horizontal,
      s.mytaglist,
    },
    {
      -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      -- mytextclock, -- Middle widget
    },
  }
end

local function wibox_per_screen(s)
  if s == screen.primary then
    wibox_primary(s)
  else
    wibox_secondary(s)
  end
end

awful.screen.connect_for_each_screen(function(s)
  wibox_per_screen(s)
end)
