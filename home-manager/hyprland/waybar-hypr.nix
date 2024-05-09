{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.waybar.settings.mainBar = {
    modules-left = ["hyprland/workspaces"];
    "hyprland/workspaces" = {
      format = "{id}: {name}";
    };
  };
}
