{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./hyprland/default.nix
  ];
  home.packages = with pkgs; [
    slurp
    wayshot
  ];

  wayland.windowManager = {
    sway.enable = false;
    hyprland.enable = true;
  };

  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings.mainBar = {
      layer = "top";
      position = "top";
      height = 30;
      modules-left = []; # hyprland file places workspaces here
      modules-center = ["clock"];
      modules-right = ["network" "wireplumber" "cpu" "temperature" "temperature#gpu"];
      "cpu" = {
        format = "[ CPU: {usage}%,";
      };
      "temperature" = {
        format = "{temperatureC}°C ]";
      };
      "temperature#gpu" = {
        hwmon-path = "/sys/devices/pci0000:00/0000:00:03.1/0000:2b:00.0/0000:2c:00.0/0000:2d:00.0/hwmon/hwmon1/temp2_input";
        format = "[ GPU: {temperatureC}°C ]";
      };
      "tray" = {
        icon-size = 21;
        spacing = 10;
        show-passive-items = true;
      };
      "wireplumber" = {
        format = "[ Vol: {volume}% ]";
        on-click = "pavucontrol";
      };
      "mpris" = {
        format = "Playing: {title} {position} / {length}";
        format-paused = "Paused: {title}";
      };
      "network" = {
        format = "[ Net: {essid} {signalStrength}% ]";
      };
      "clock" = {
        format = "{:%I:%M %p, %a, %b %d}";
      };
    };
  };

  services.dunst = {
    enable = true;
    settings.global = {
      follow = "mouse";
      layer = "overlay";
      format = "%a\n<b>%s</b>\n%b";
      history_length = 50;

      width = 400;
      height = 300;
      corner_radius = 15;

      mouse_left_click = "do_action, close_current";
      mouse_middle_click = "close_all";
      mouse_right_click = "close_current";
    };
  };

  services.clipman = {
    enable = true;
  };
}
