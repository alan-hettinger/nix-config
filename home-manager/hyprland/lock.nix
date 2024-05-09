{
  config,
  lib,
  pkgs,
  hyprlock,
  ...
}: {
  imports = [hyprlock.homeManagerModules.hyprlock];
  programs.hyprlock = {
    enable = true;
    general = {
      grace = 5;
      hide_cursor = false;
    };
    backgrounds = [
      {
        path = toString (config.lib.stylix.pixel "base00");
        blur_size = 0;
        noise = 0.0;
        contrast = 0.0;
        brightness = 0.0;
        vibrancy = 0.0;
        vibrancy_darkness = 0.0;
      }
    ];
    input-fields = [
      {
        fade_on_empty = false;
      }
    ];
    labels = [
      {
        text = "Hello, $USER.";
      }
      {
        text = "$TIME";
        font_size = 48;
        position = {
          x = 0;
          y = 160;
        };
      }
    ];
  };
}
