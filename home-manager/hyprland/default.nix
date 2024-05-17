{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = lib.helperFunctions.getNixFilesFromDir ./.;
  home.packages = with pkgs; [
    hyprpaper
    wl-clipboard
  ];

  wayland.windowManager.hyprland = {
    systemd = {
      enable = true;
      variables = ["--all"];
    };
    settings = {
      general = {
        resize_on_border = false;
      };
      debug.disable_logs = false;
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
        "wl-paste --type text --watch cliphist store"
        "wl-paste --type image --watch cliphist store"
      ];
    };
  };
}
