{ config, lib, pkgs, ... }: {
  wayland.windowManager.hyprland = {
    enable = true;
    systemdIntegration = true;
    recommendedEnvironment = true;
    # extraConfig = ## dir for config file
  };
  programs.waybar = {
    enable = true;
    settings = {

    };
    # style = ; ## dir of the css style
    systemd.enable = true;
  };
}
