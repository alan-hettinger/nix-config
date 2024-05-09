{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    general.allow_tearing = false;
    misc.vrr = 2;
    monitor = ["DP-1, highrr, auto, auto"];
  };
}
