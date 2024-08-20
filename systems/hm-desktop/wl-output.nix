{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    misc.vrr = 1;
    monitor = ["DP-1, highrr, auto, auto"];
  };
}
