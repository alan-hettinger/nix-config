{config, ...}: let
  wallpaperLavender = config.lib.stylix.pixel "base07";
  wallpaperBase = config.lib.stylix.pixel "base00";
  wallpaperBlue = config.lib.stylix.pixel "base0D";

  themeColors = {
    # FIXME this should be set by stylix
    text = "rgb(cad3f5)";
    bgInactive = "rgb(24273a)";
    bgActive = "rgb(181926)";
    borderMaximized = "rgb(ee99a0)";
  };
in {
  wayland.windowManager.hyprland.settings = {
    general = {
      gaps_in = 5;
      gaps_out = 15;
      border_size = 3;
    };
    cursor = {
      inactive_timeout = 30;
    };
    misc = {
      disable_hyprland_logo = true;
      force_default_wallpaper = 0;
    };
    decoration = {
      rounding = 15;
      blur = {
        enabled = true;
      };
      drop_shadow = true;
      shadow_ignore_window = true;
      shadow_offset = "10 15";
      shadow_range = 4;
      shadow_render_power = 3;
      shadow_scale = 1.0;

      active_opacity = 1.0;
      inactive_opacity = 1.0;
      dim_inactive = true;
      dim_strength = 0.3;
    };
    animations = {
      enabled = true;
      first_launch_animation = false;
    };
    group = {
      groupbar = {
        enabled = true;
        font_family = "Source Sans Variable";
        font_size = 16;
        height = 20;

        text_color = themeColors.text;
        # "col.active" = themeColors.bgActive;
        # "col.inactive" = themeColors.bgInactive;
        # "col.locked_active" = themeColors.bgActive;
        # "col.locked_inactive" = themeColors.bgInactive;
      };
    };
    exec-once = [
      "hyprpaper"
    ];
    windowrulev2 = [
      "bordercolor ${themeColors.borderMaximized}, fullscreen:1"
      "rounding 0, fullscreen:1"
      "noblur, fullscreen:1"
      "noshadow, fullscreen:1"
    ];
  };

  xdg.configFile."hypr/hyprpaper.conf".text = ''
    preload = ${wallpaperLavender}
    preload = ${wallpaperBase}
    preload = ${wallpaperBlue}
    wallpaper = ,${wallpaperLavender}
    splash = false
  '';
}
