{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  programs.hyprland = {
    enable = true;
    # package = inputs.hyprland.packages.${pkgs.system}.hyprland;
  };
}
