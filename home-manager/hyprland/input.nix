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
        kb_options = "caps:escape";
        kb_layout = "us";
        numlock_by_default = true;
      };
    };
  };
}
