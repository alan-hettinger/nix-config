{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    input = {
      accel_profile = "adaptive";
      sensitivity = 0.3;
      scroll_method = "2fg";
      touchpad = {
        disable_while_typing = true;
        natural_scroll = true;
        clickfinger_behavior = true;
      };
    };
  };
}
