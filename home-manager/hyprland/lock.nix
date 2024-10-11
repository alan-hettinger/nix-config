{
  config,
  lib,
  pkgs,
  ...
}: let
  stylixColor = config.lib.stylix.colors;
in {
  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        grace = 5;
        hide_cursor = true;
        no_fade_out = true;
      };
      background = [
        {
          color = "rgb(${stylixColor.base00})";
          blur_size = 0;
          noise = 0.0;
          contrast = 0.0;
          brightness = 0.0;
          vibrancy = 0.0;
          vibrancy_darkness = 0.0;
        }
      ];
      input-field = [
        {
          fade_on_empty = false;
          outer_color = "rgb(${stylixColor.base01})";
          inner_color = "rgb(${stylixColor.base05})";
          font_color = "rgb(${stylixColor.base00})";
          position = "0, -75";
          halign = "center";
          valign = "center";
        }
      ];
      label = [
        # {
        #   text = "Hello, $USER.";
        #   color = "rgb(${stylixColor.base07})";
        # }
        {
          text = "$TIME";
          # TODO when https://github.com/hyprwm/hyprlock/pull/500
          # is in release version, change to "$TIME12"
          color = "rgb(${stylixColor.base07})";
          font_size = 48;
          position = "0, 75";
          halign = "center";
          valign = "center";
        }
      ];
    };
  };
}
