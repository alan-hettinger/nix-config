{
  config,
  lib,
  pkgs,
  ...
}: let
  browser = "firefox";
  terminal = "alacritty -e tmux attach";
  launcher = "rofi -show combi -location 1 -display-combi '>>'";
  windowswitcher = "rofi -show window -sidebar-mode";
  editor = "emacsclient -c";
  screenshotPath = "~/Pictures/screenshots";
  screenshot = "wayshot -s \"$(slurp)\" -f ${screenshotPath}/$(date --iso-8601=seconds).jpg";
  screenshotFull = "wayshot -f ${screenshotPath}/$(date --iso-8601=seconds).jpg";
  filemanager = "dolphin";
  browser2 = "brave";
  # powerMenu = "rofi -show power-menu -modi power-menu:rofi-power-menu -location 3";
  powerMenu = "wlogout";
  clipboard = "clipman pick -t rofi";
  lock = "hyprlock";
in {
  home.packages = with pkgs; [killall];
  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";

    bindm = [
      "$mod, mouse:272, movewindow"
      "$mod, mouse:273, resizewindow"
    ];

    bind =
      [
        "$mod, X, exec, [workspace 1] ${browser}"
        "$mod SHIFT, X, exec, [workspace 1] ${browser2}"
        "$mod, Return, exec, ${terminal}"
        "$mod, Z, exec, ${filemanager}"
        "$mod, C, exec, [workspace 6] discord"
        "$mod, V, exec, ${editor}"
        "$mod, D, exec, ${launcher}"
        "$mod, P, exec, ${screenshot}"
        "$mod SHIFT, P, exec, ${screenshotFull}"
        "$mod, W, exec, ${windowswitcher}"
        "$mod, Escape, exec, ${powerMenu}"
        "$mod, comma, exec, ${clipboard}"
        "$mod, grave, exec, ${lock}"

        "$mod, Q, killactive"

        "$mod, H, layoutmsg, mfact, -0.2"
        "$mod, J, cyclenext"
        "$mod SHIFT, J, swapnext"
        "$mod, K, cyclenext, prev"
        "$mod SHIFT, K, swapnext, prev"
        "$mod, L, layoutmsg, mfact, +0.2"

        "$mod, F, fullscreen"
        "$mod SHIFT, F, fakefullscreen"

        "$mod CTRL, Space, togglefloating, active"
        "$mod, Tab, workspace, e+1"
        "$mod SHIFT, Tab, workspace, e-1"

        "$mod, T, togglegroup"
        "$mod, Right, changegroupactive, f"
        "$mod, Left, changegroupactive, b"
        "$mod SHIFT, Left, moveintogroup, l"
        "$mod SHIFT, Right, moveintogroup, right"
        "$mod SHIFT, Up, moveintogroup, up"
        "$mod SHIFT, Down, moveintogroup, down"

        # TODO mod,b toggles bar
        "$mod, b, exec, killall -SIGUSR1 waybar"
        # TODO mod,= reduces gap size
        # TODO mod,- increases gap size
        # TODO mod,CTRL,j|k cycles screens
        # TODO mod,CTRL,Enter launches default apps
        # TODO mod,Shift,q quits
        # TODO mod,/ toggles tray visibility
        # TODO mod,F10|F11|F12 control volume
        # TODO mod,n controls minimized
      ]
      ++ (
        builtins.concatLists (builtins.genList (
            x: let
              ws = let
                c = (x + 1) / 10;
              in
                builtins.toString (x + 1 - (c * 10));
            in [
              "$mod, ${ws}, workspace, ${toString (x + 1)}"
              "$mod SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}"
            ]
          )
          10)
      );
  };
}
