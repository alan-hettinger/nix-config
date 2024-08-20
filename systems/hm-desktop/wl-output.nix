{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    misc.vrr = 1;
    monitor = ["DP-1, highrr, auto, auto"];
  };
  programs.waybar = {
    settings.mainBar = {
      modules-right = [
        "temperature#gpu"
      ];
      "temperature#gpu" = {
        hwmon-path = "/sys/devices/pci0000:00/0000:00:03.1/0000:2b:00.0/0000:2c:00.0/0000:2d:00.0/hwmon/hwmon1/temp2_input";
        format = "[ GPU: {temperatureC}°C ]";
      };
    };
  };
}
