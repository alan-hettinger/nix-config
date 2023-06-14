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
      layer = "top";
      position = "left"; # bar position - top/bottom/left/right
      modules-left =
        [ "clock" "clock#clock2" "clock#clock3" "sway/workspaces" "sway/mode" ];
      modules-center = [ ];
      modules-right = [ "battery" "memory" "temperature" "tray" ];

      "sway/workspaces" = {
        disable-scroll = true;
        all-outputs = true;
        format = "{icon}";

        persistent_workspaces = {
          "1" = [ ];
          "2" = [ ];
          "3" = [ ];
          "4" = [ ];
          "5" = [ ];
        };
        format-icons = {
          "1" = "";
          "2" = "";
          "3" = "";
          "4" = "";
          "5" = "@";
        };
        align = 0;
      };
      "sway/mode" = { format = ''<span style="italic">{}<span>''; };
      "tray" = {
        rotate = 90;
        icon-size = 20;
      };
      "clock".format = " {:%I}";
      "clock#clock2".format = " {:%M}";
      "clock#clock3".format = " {:%p}";
      "battery".format = "{capacity}%";
      "cpu" = {
        interval = 1;
        format = ": {usage}%";
        tooltip = false;
      };
      "temperature" = {
        interval = 4;
        hwmon-path =
          /sys/class/hwmon/hwmon3/temp1_input; # TODO should this be a string?
        format = "{temperatureC}°";
      };
      "network" = {
        format-wifi = "  {essid}";
        format-ethernet = "{ifname}: {ipaddr}/{cidr} ";
        format-linked = "{ifname} (No IP) ";
        format-disconnected = "";
        format-alt = "{ifname}: {ipaddr}/{cidr}";
        family = "ipv4";
        tooltip-format-wifi = ''
            {ifname} @ {essid}
          IP: {ipaddr}
          Strength: {signalStrength}%
          Freq: {frequency}MHz
           {bandwidthUpBits}  {bandwidthDownBits}'';
        tooltip-format-ethernet = ''
           {ifname}
          IP: {ipaddr}
           {bandwidthUpBits}  {bandwidthDownBits}'';
      };
      "memory" = {
        interval = 30;
        format = "{}%";
      };
    };
    systemd.enable = true;
  };

  wayland.windowManager.sway = { # keeping this config for posterity
    enable = false;
    config = {

    };

  };
}
