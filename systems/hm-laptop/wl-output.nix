{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    monitor = ["eDP-1, 1920x1080, auto, 1"];
  };
}
