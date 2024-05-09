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
    hyprpaper
    slurp
    wayshot
  ];

  wayland.windowManager = {
    sway.enable = false;
    hyprland.enable = true;
  };
  programs.waybar.enable = true;

  programs.waybar = {
    systemd.enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 30;
        modules-left = ["hyprland/workspaces"];
        modules-center = ["clock"];
        modules-right = ["systemd-failed-units" "network" "wireplumber" "cpu" "temperature" "temperature#gpu"];
        "hyprland/workspaces" = {
          format = "{id}: {name}";
        };
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
  };

  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          width = 400;
          height = 300;
        };
      };
    };
    clipman = {
      enable = true;
    };
  };
}
