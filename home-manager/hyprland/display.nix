{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    general.allow_tearing = true;
  };
}
