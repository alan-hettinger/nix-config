{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    input = {
      accel_profile = "flat";
      sensitivity = 0.3;
    };
  };
}
