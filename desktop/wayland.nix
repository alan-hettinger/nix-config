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
    systemd = {
      setPath.enable = true;
    };
  };
  programs.hyprlock.enable = true;
  security.pam.services.hyprlock = {
  };
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
