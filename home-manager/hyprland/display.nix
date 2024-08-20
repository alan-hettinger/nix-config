{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    general.allow_tearing = true;
    misc.vrr = 1;
    monitor = ["DP-1, highrr, auto, auto"];
  };
}
