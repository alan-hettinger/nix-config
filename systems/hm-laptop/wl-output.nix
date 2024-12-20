{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    monitor = ["eDP-1, 1920x1080, auto, 1"];
  };
  programs.waybar = {
    settings.mainBar = {
      modules-right = [
        "custom/separator"
        "battery"
      ];
      "battery" = {
        format = "󰁹 {capacity}% ";
      };
    };
  };
}
