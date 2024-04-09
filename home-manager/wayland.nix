{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager = {
    sway.enable = false;
    hyprland.enable = true;
  };
  programs.waybar.enable = true;

  wayland.windowManager.hyprland = {
    systemdIntegration = true;
    recommendedEnvironment = true;
    extraConfig = {};
  };

  programs.waybar = {
    settings = {
      layer = "top";
      position = "left"; # bar position - top/bottom/left/right
      modules-left = ["clock" "clock#clock2" "clock#clock3" "sway/workspaces" "sway/mode"];
      modules-center = [];
      modules-right = ["battery" "memory" "temperature" "tray"];

      "sway/workspaces" = {
        disable-scroll = true;
        all-outputs = true;
        format = "{icon}";

        persistent_workspaces = {
          "1" = [];
          "2" = [];
          "3" = [];
          "4" = [];
          "5" = [];
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
      "sway/mode" = {format = ''<span style="italic">{}<span>'';};
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

  wayland.windowManager.sway = {
    # keeping this config for posterity
    config = {
      focus = {
        followMouse = false;
        mouseWarping = false;
      };
      fonts = {
      };
      bars.waybar.command = "waybar";
      gaps = {
        # gap sizes are ints
        # bottom = ;
        # horizontal = ;
        inner = 10;
        # outer = ;
        # right = ;
        # top = ;
        # vertical = ;
        smartBorders = "off";
        smartGaps = "off";
      };

      input = {
        # attribute set of strings
      };
      modifier = "Mod4";
      keybindings = {
        "$mod+Return" = "exec $term";
        "$mod+q" = "kill";
        "$mod+d" = "exec $menu";
        "$mod+Ctrl+r" = "reload";
        "$mod+p" = "exec $scshot";
        "$mod+Shift+p" = "exec $regionshot";
        "$mod+b" = "splith";
        "$mod+v" = "splitv";
        "$mod+s" = "layout stacking";
        "$mod+t" = "layout tabbed";
        "$mod+w" = "layout splitv";
        "$mod+e" = "layout toggle splith tabbed";
        "$mod+f" = "fullscreen";
        "$mod+Shift+space" = "floating toggle";
        "$mod+space" = "focus mode_toggle";
        "$mod+a" = "focus parent";
        "$mod+r" = "mode 'resize'";
        "$mod+Escape" = "exec $powermenu";
        "$mod+x" = "exec $browser";
        "$mod+z" = "exec $filemgr";
      };
      output = {
      };
      terminal = "alacritty";
      menu = "rofi -show drun";
      window = {
        border = 2;
        hideEdgeBorders = "none";
        titlebar = false;
      };
      workspaceLayout = "default";

      extraConfig = ''
        set $scshot exec grim
        set $regionshot exec grim -g "$(slurp)"

        set $powermenu exec wlogout -p layer-shell
        set $browser exec 'firefox'
        set $filemgr exec nemo

      '';
    };
  };
}
