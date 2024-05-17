{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.waybar.settings.mainBar = {
    modules-left = ["hyprland/workspaces" "hyprland/submap"];
    "hyprland/workspaces" = {
      format = "{id}: {name}";
      show-special = false;
    };
  };
}
