{ config, lib, pkgs, ... }: {

  gtk.font = {
    name = "Source Sans Pro";
    size = 16;
  };
  fonts.fontconfig.enable = true;
  programs.alacritty.settings.font = {
    normal = {
      family = "mononoki";
      style = "Regular";
    };
    bold = {
      family = "mononoki";
      style = "Bold";
    };
    italic = {
      family = "mononoki";
      style = "Italic";
    };
    size = 16;
    builtin_box_drawing = true;
  };
}
