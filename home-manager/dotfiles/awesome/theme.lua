local theme_assets                              = require("beautiful.theme_assets")
local xresources                                = require("beautiful.xresources")
local dpi                                       = xresources.apply_dpi
local gfs                                       = require("gears.filesystem")
local themes_path                               = gfs.get_themes_dir()
local assets_dir                                = "~/.config/awesome/assets/"
local theme                                     = {}
local cat                                       = require("catppuccin")

-- Font variables defined by me:
theme.mono_font                                 = "Jetbrains Mono Nerd Font 14"
theme.variable_font                             = "Liberation Sans 15"

-- colors added by me to modularize theming:
theme.red                                       = cat.red
theme.blue                                      = cat.blue
theme.lightRed                                  = cat.maroon
theme.yellow                                    = cat.yellow
theme.lightYellow                               = cat.rosewater
theme.baseBlack                                 = cat.base
theme.baseWhite                                 = cat.text

-- basic theme colors
theme.font                                      = theme.variable_font
theme.font_color                                = theme.baseWhite

theme.bg_normal                                 = theme.baseBlack
theme.bg_focus                                  = cat.surface2
theme.bg_urgent                                 = theme.red
theme.bg_minimize                               = cat.surface1

theme.fg_normal                                 = theme.baseWhite
theme.fg_focus                                  = theme.lightYellow
theme.fg_urgent                                 = theme.red
theme.fg_minimize                               = cat.surface1

theme.useless_gap                               = dpi(8)
theme.border_width                              = dpi(3)
theme.gap_single_client                         = true
theme.border_normal                             = cat.surface0
theme.border_focus                              = theme.blue
theme.border_marked                             = theme.border_normal

-- notification theme
theme.notification_bg                           = theme.bg_normal
theme.notification_fg                           = theme.font_color
theme.notification_border_width                 = dpi(2)
theme.notification_border_color                 = theme.border_focus
theme.notification_opacity                      = 0.9
theme.notification_margin                       = 10
theme.notification_max_width                    = 500
theme.notification_max_height                   = 1000
theme.notification_icon_size                    = 200

-- variables for layout
theme.master_width_factor                       = 0.55
theme.master_fill_policy                        = master_width_factor
theme.column_count                              = 1
theme.master_count                              = 1

-- tasklist theme
theme.tasklist_spacing                          = 8
theme.tasklist_bg_normal                        = theme.bg_normal
theme.tasklist_fg_normal                        = theme.fg_normal
theme.tasklist_bg_focus                         = cat.surface1
theme.tasklist_fg_focus                         = theme.font_color
theme.tasklist_disable_icon                     = true
theme.tasklist_bg_minimize                      = cat.mantle
theme.tasklist_shape_border_width               = 2
theme.tasklist_shape_border_color               = theme.border_normal
theme.tasklist_shape_border_width_focus         = 2
theme.tasklist_shape_border_color_focus         = theme.border_focus

-- titlebar theme
theme.titlebar_fg_normal                        = theme.fg_normal
theme.titlebar_bg_normal                        = cat.crust
theme.titlebar_fg_focus                         = theme.font_color
theme.titlebar_bg_focus                         = cat.crust

theme.hotkeys_font                              = theme.mono_font -- "Source Sans 3 VF 16"
theme.hotkeys_desription_font                   = theme.mono_font -- "Source Sans 3 VF 14"

-- Generate taglist squares:
local taglist_square_size                       = dpi(0)
theme.taglist_squares_sel                       = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel                     = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_font                              = theme.mono_font
theme.taglist_bg_focus                          = cat.surface1
theme.taglist_bg_urgent                         = cat.surface0
theme.taglist_bg_occupied                       = cat.surface0
theme.taglist_bg_empty                          = cat.base
theme.taglist_shape_border_width                = 2
theme.taglist_shape_border_color                = cat.surface1
theme.taglist_shape_border_color_focus          = cat.overlay0
theme.taglist_shape_border_color_empty          = cat.base

theme.menu_submenu_icon                         = themes_path .. "default/submenu.png"
theme.menu_height                               = dpi(15)
theme.menu_width                                = dpi(100)

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

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
