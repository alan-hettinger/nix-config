local theme_assets                      = require("beautiful.theme_assets")
local xresources                        = require("beautiful.xresources")
local dpi                               = xresources.apply_dpi

local gfs                               = require("gears.filesystem")
local themes_path                       = gfs.get_themes_dir()
local gears                             = require "gears"

local device                            = RC.vars.device

local assets_dir                        = "~/.config/awesome/assets/"

local theme                             = {}

local lain                              = require("lain")

-- Font variables defined by me:
theme.mono_font                         = "Jetbrains Mono Nerd Font 14"
theme.variable_font                     = "Liberation Sans 15"

theme.font                              = theme.variable_font
theme.font_color                        = "#cdd6f4" -- catppuccin macchiato text

theme.bg_normal                         = "#24273a" -- catppuccin macchiato base
theme.bg_focus                          = "#535d6c" -- not a custom theme ?
theme.bg_urgent                         = "#ff0000"
theme.bg_minimize                       = "#444444"

theme.fg_normal                         = "#cad3f5" -- catppuccin macchiato text
theme.fg_focus                          = "#f4dbd6" -- catppuccin macchiato rosewater
theme.fg_urgent                         = "#ed8796" -- catppuccin macchiato red
theme.fg_minimize                       = "#9399b2"

theme.useless_gap                       = dpi(5)
theme.border_width                      = dpi(2)
theme.gap_single_client                 = true
theme.border_normal                     = "#363a4f" -- catppuccin macchiato surface0
theme.border_focus                      = "#8aadf4" -- macchiato blue
-- "#9399b2" -- mocha overlay2
theme.border_marked                     = theme.border_normal

-- notification theme
theme.notification_bg                   = theme.bg_normal
theme.notification_fg                   = theme.font_color
theme.notification_border_width         = dpi(2)
theme.notification_border_color         = theme.border_focus
theme.notification_opacity              = 0.9
theme.notification_margin               = 10
theme.notification_max_width            = 500
theme.notification_max_height           = 1000
theme.notification_icon_size            = 200

-- tasklist theme
theme.tasklist_spacing                  = 8
theme.tasklist_bg_normal                = theme.bg_normal
theme.tasklist_fg_normal                = theme.fg_normal
theme.tasklist_bg_focus                 = "#494d64"
theme.tasklist_fg_focus                 = theme.font_color
theme.tasklist_disable_icon             = true
theme.tasklist_bg_minimize              = "#1e2030"
theme.tasklist_shape_border_width       = 2
theme.tasklist_shape_border_color       = theme.border_normal
theme.tasklist_shape_border_width_focus = 2
theme.tasklist_shape_border_color_focus = theme.border_focus

-- titlebar theme
theme.titlebar_fg_normal                = theme.fg_normal
theme.titlebar_bg_normal                = "#181926"       -- macchiato crust
theme.titlebar_fg_focus                 = theme.font_color
theme.titlebar_bg_focus                 = "#181926"       -- macchiato crust

theme.hotkeys_font                      = theme.mono_font -- "Source Sans 3 VF 16"
theme.hotkeys_desription_font           = theme.mono_font -- "Source Sans 3 VF 14"

-- Generate taglist squares:
local taglist_square_size               = dpi(0)
theme.taglist_squares_sel               = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel             = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_font                      = theme.mono_font
theme.taglist_bg_focus                  = "#494d64"
theme.taglist_bg_urgent                 = "#363a4f"
theme.taglist_bg_occupied               = "#313244"
theme.taglist_bg_empty                  = "#24273a"
theme.taglist_shape_border_width        = 2
theme.taglist_shape_border_color        = "#494d64"
theme.taglist_shape_border_color_focus  = "#6e738d"
theme.taglist_shape_border_color_empty  = "#24273a"

-- theme.taglist_spacing = 12
-- theme.taglist_shape = "gears.shape.rounded_rect"
-- theme.taglist_shape_border_width = 3
-- theme.taglist_shape_border_color = theme.border_normal
-- theme.taglist_shape_border_width_focus = 3
-- theme.taglist_shape_border_color_focus = theme.border_focus


-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon                         = themes_path .. "default/submenu.png"
theme.menu_height                               = dpi(15)
theme.menu_width                                = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal              = assets_dir .. "close_1.png"
theme.titlebar_close_button_focus               = assets_dir .. "close_2.png"

theme.titlebar_ontop_button_normal_inactive     = assets_dir .. "maximize_1.png"
theme.titlebar_ontop_button_focus_inactive      = assets_dir .. "maximize_1.png"
theme.titlebar_ontop_button_normal_active       = assets_dir .. "maximize_2.png"
theme.titlebar_ontop_button_focus_active        = assets_dir .. "maximize_2.png"

theme.titlebar_sticky_button_normal_inactive    = assets_dir .. "maximize_1.png"
theme.titlebar_sticky_button_focus_inactive     = assets_dir .. "maximize_1.png"
theme.titlebar_sticky_button_normal_active      = assets_dir .. "maximize_2.png"
theme.titlebar_sticky_button_focus_active       = assets_dir .. "maximize_2.png"

theme.titlebar_floating_button_normal_inactive  = assets_dir .. "maximize_1.png"
theme.titlebar_floating_button_focus_inactive   = assets_dir .. "maximize_1.png"
theme.titlebar_floating_button_normal_active    = assets_dir .. "maximize_2.png"
theme.titlebar_floating_button_focus_active     = assets_dir .. "maximize_2.png"

theme.titlebar_maximized_button_normal_inactive = assets_dir .. "maximize_1.png"
theme.titlebar_maximized_button_focus_inactive  = assets_dir .. "maximize_1.png"
theme.titlebar_maximized_button_normal_active   = assets_dir .. "maximize_2.png"
theme.titlebar_maximized_button_focus_active    = assets_dir .. "maximize_2.png"


theme.titlebar_minimize_button_normal = themes_path .. "default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path .. "default/titlebar/minimize_focus.png"


theme.wallpaper          = assets_dir .. "wallpaper.png"

-- You can use your own layout icons like this:
theme.layout_fairh       = themes_path .. "default/layouts/fairhw.png"
theme.layout_fairv       = themes_path .. "default/layouts/fairvw.png"
theme.layout_floating    = themes_path .. "default/layouts/floatingw.png"
theme.layout_magnifier   = themes_path .. "default/layouts/magnifierw.png"
theme.layout_max         = themes_path .. "default/layouts/maxw.png"
theme.layout_fullscreen  = themes_path .. "default/layouts/fullscreenw.png"
theme.layout_tilebottom  = themes_path .. "default/layouts/tilebottomw.png"
theme.layout_tileleft    = themes_path .. "default/layouts/tileleftw.png"
theme.layout_tile        = themes_path .. "default/layouts/tilew.png"
theme.layout_tiletop     = themes_path .. "default/layouts/tiletopw.png"
theme.layout_spiral      = themes_path .. "default/layouts/spiralw.png"
theme.layout_dwindle     = themes_path .. "default/layouts/dwindlew.png"
theme.layout_cornernw    = themes_path .. "default/layouts/cornernww.png"
theme.layout_cornerne    = themes_path .. "default/layouts/cornernew.png"
theme.layout_cornersw    = themes_path .. "default/layouts/cornersww.png"
theme.layout_cornerse    = themes_path .. "default/layouts/cornersew.png"
theme.lain_icons         = os.getenv("HOME") ..
    "/.config/awesome/lain/icons/layout/default/"
theme.layout_termfair    = theme.lain_icons .. "termfair.png"
theme.layout_centerfair  = theme.lain_icons .. "centerfair.png"  -- termfair.center
theme.layout_cascade     = theme.lain_icons .. "cascade.png"
theme.layout_cascadetile = theme.lain_icons .. "cascadetile.png" -- cascade.tile
theme.layout_centerwork  = theme.lain_icons .. "centerwork.png"
theme.layout_centerworkh = theme.lain_icons .. "centerworkh.png" -- centerwork.horizontal


-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = "/usr/share/icons/Papirus-Dark"

-- systray_icon_spacing = (4)
theme.tag_preview_widget_border_radius = 0        -- Border radius of the widget (With AA)
theme.tag_preview_client_border_radius = 0        -- Border radius of each client in the widget (With AA)
theme.tag_preview_client_opacity = 0.5            -- Opacity of each client
theme.tag_preview_client_bg = "#000000"           -- The bg color of each client
theme.tag_preview_client_border_color = "#ffffff" -- The border color of each client
theme.tag_preview_client_border_width = 3         -- The border width of each client
theme.tag_preview_widget_bg = "#000000"           -- The bg color of the widget
theme.tag_preview_widget_border_color = "#ffffff" -- The border color of the widget
theme.tag_preview_widget_border_width = 3         -- The border width of the widget
theme.tag_preview_widget_margin = 0               -- The margin of the widget

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
