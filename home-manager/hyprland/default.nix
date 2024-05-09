{
  config,
  lib,
  pkgs,
  ...
}: let
  # wallpaper = ./dotfiles/awesome/assets/wall-mountains-catppuccin.png;
  wallpaperLavender = config.lib.stylix.pixel "base07";
  wallpaperBase = config.lib.stylix.pixel "base00";
  wallpaperBlue = config.lib.stylix.pixel "base0D";
in {
  imports = lib.helperFunctions.getNixFilesFromDir ./.;

  wayland.windowManager.hyprland = {
    systemd = {
      enable = true;
      variables = ["--all"];
    };
    settings = {
      general = {
        gaps_in = 5;
        gaps_out = 15;
        border_size = 3;
        layout = "master";
        allow_tearing = false;
        resize_on_border = false;
      };
      env = [
        "GDK_BACKEND, wayland, x11, *"
        "QT_QPA_PLATFORM, wayland; xcb"
        "QT_AUTO_SCREEN_SCALE_FACTOR, 1"
        "QT_WAYLAND_DISABLE_WINDOWDECORATION, 1"
        "XDG_CURRENT_DESKTOP, Hyprland"
        "XDG_SESSION_TYPE, wayland"
        "XDG_SESSION_DESKTOP, Hyprland"
        "XCURSOR_SIZE, 48"
      ];
      exec-once = [
        "clipman clear"
        "hyprpaper"
      ];
      input = {
        follow_mouse = 2;
        accel_profile = "flat";
        kb_options = "caps:swapescape";
        kb_layout = "us";
        numlock_by_default = true;
        sensitivity = 0.3;
      };
      misc = {
        vrr = 2;
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
      monitor = ["DP-1, highrr, auto, auto"];
      workspace = [
        "1, default:true, defaultName:web"
        "2, defaultName:code"
        "3, defaultName:notes"
        "4, defaultName:doc"
        "5, defaultName:mail"
        "6, layoutopt:orientation:center, defaultName:chat"
        "7, defaultName:files"
        "8, defaultName:media"
        "9, defaultName:game"
        "10, defaultName:"
      ];
      master = {
        mfact = 0.55;
        new_is_master = false;
        new_on_top = false;
        no_gaps_when_only = 0;
        always_center_master = true;
        inherit_fullscreen = false;
      };
    };
  };
  xdg.configFile."hypr/hyprpaper.conf".text = ''
    preload = ${wallpaperLavender}
    preload = ${wallpaperBase}
    preload = ${wallpaperBlue}
    wallpaper = ,${wallpaperLavender}
    splash = false
  '';
}
