{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland = {
    settings = {
      input = {
        follow_mouse = 2;
        accel_profile = "flat";
        kb_options = "caps:swapescape";
        kb_layout = "us";
        numlock_by_default = true;
        sensitivity = 0.3;
      };
    };
  };
}
