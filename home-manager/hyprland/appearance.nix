{config, ...}: let
  stylixColor = config.lib.stylix.colors;
  wallpaperLavender = config.lib.stylix.pixel "base07";
  wallpaperBase = config.lib.stylix.pixel "base00";
  wallpaperBlue = config.lib.stylix.pixel "base0D";
  colors = {
    foreground = stylixColor.base05;
  };

  themeColors = {
    # FIXME this should be set by stylix
    text = "rgb(${colors.foreground})";
    borderMaximized = "rgb(ee99a0)"; # FIXME 2024-11 this color doesn't appear in theme?
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
      shadow = {
        enabled = true;
        range = 4;
        render_power = 3;
        ignore_window = true;
        offset = "8 12";
        scale = 1.0;
      };

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
