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
  };
}
