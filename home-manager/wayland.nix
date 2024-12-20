{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [
    ./hyprland
  ];
  home.packages = with pkgs; [
    slurp
    wayshot

    brightnessctl
  ];

  wayland.windowManager = {
    sway.enable = false;
    hyprland.enable = true;
  };

  programs.waybar = {
    enable = true;
    systemd = {
      enable = true;
      target = "hyprland-session.target";
    };
    settings.mainBar = {
      layer = "top";
      position = "top";
      height = 30;
      modules-left = []; # hyprland file places workspaces here
      modules-center = ["clock"];
      modules-right = [
        "group/tray"
        "custom/separator"
        "group/stats"
      ]; # system-specific files add to this
      "cpu" = {
        format = "   {usage}%,";
      };
      "custom/separator" = {
        format = "";
        tooltip = false;
      };
      "temperature" = {
        format = "{temperatureC}°C";
      };
      "group/tray" = {
        orientation = "inherit";
        drawer = {
          transition-left-to-right = false;
        };
        modules = [
          "custom/trayIcon"
          "tray"
        ];
      };
      "custom/trayIcon" = {
        format = "  ";
        tooltip = false;
      };
      "tray" = {
        icon-size = 21;
        spacing = 10;
        show-passive-items = true;
      };
      "group/stats" = {
        orientation = "inherit";
        drawer = {
          transition-left-to-right = false;
          click-to-reveal = true;
        };
        modules = [
          "custom/statIcon"
          "network"
          "custom/separator"
          "wireplumber"
          "custom/separator"
          "cpu"
          "temperature"
        ];
      };
      "custom/statIcon" = {
        format = "  ";
        tooltip-format = "System stats";
      };
      "wireplumber" = {
        format = "  {volume}%";
        format-muted = " ";
        on-click = "pavucontrol";
      };
      "mpris" = {
        format = "Playing: {title} {position} / {length}";
        format-paused = "Paused: {title}";
      };
      "network" = let
        bandwidthStr = "↓{bandwidthDownBytes} ↑{bandwidthUpBytes}";
      in {
        format-wifi = "󰤨  {essid}";
        format-ethernet = "Net: ${bandwidthStr}";
        tooltip = true;
        tooltip-format = "{signalStrength}% ${bandwidthStr}";
      };
      "clock" = {
        format = "{:%I:%M %p, %a, %b %d}";
      };
    };
  };

  programs.waybar.style =
    ## Custom waybar style. Use lib.mkAfter to append to stylix file.
    lib.mkAfter ''
      /* Manual changes: */
      window#waybar {
          border: 3px solid ${config.lib.stylix.colors.withHashtag.base01};
      }
    '';

  services.dunst = {
    enable = true;
    settings.global = {
      follow = "keyboard";
      layer = "overlay";
      format = "%a\\n<b>%s</b>\\n%b";
      history_length = 50;

      # width and height are either int or string "([min], [max])"
      width = 400;
      height = "(0, 300)";
      corner_radius = 15;

      mouse_left_click = "do_action, close_current";
      mouse_middle_click = "close_all";
      mouse_right_click = "close_current";
    };
  };

  services.clipman = {
    enable = false;
  };

  services.cliphist = {
    enable = true;
    allowImages = true;
    extraOptions = [
      "-max-items"
      "100"
    ];
  };

  services.gammastep = {
    enable = false;
    dawnTime = "8:30-10:00";
    duskTime = "20:30-22:00";
    temperature = {
      day = 6500;
      night = 4000;
    };
    provider = "manual";
    latitude = 33.7;
    longitude = -84.3;
    settings = {
      general = {
        brightness-day = 1.0;
        brightness-night = 0.7;
        fade = 1;
        adjustment-method = "wayland";
      };
    };
  };

  services.wlsunset = {
    enable = true;
    # latitude = 33.7;
    # longitude = -84.3;
    sunrise = "07:00";
    sunset = "21:00";
    temperature = {
      day = 6500;
      night = 4500;
    };
  };

  services.kanshi = {
    ## TODO this is just a placeholder
    enable = false;
    extraConfig = "";
    profiles = {
      desktop = {
        outputs = [
          {
            criteria = "DP-1";
            mode = "2560x1440@144Hz";
            position = "0,0";
            adaptiveSync = true;
            scale = 1.25;
            status = "enable";
          }
        ];
        exec = [
          "echo 'hello world'"
        ];
      };
    };
  };

  services.swayosd = {
    enable = true;
    topMargin = 0.1;
  };

  programs.wlogout = {
    enable = true;
    # layout = [{}];
  };
}
