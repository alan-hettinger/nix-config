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
  powerMenu = "rofi -show power-menu -modi power-menu:rofi-power-menu -location 3";
  clipboard = "clipman pick -t rofi";
in {
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

        "$mod, Q, killactive"

        "$mod, H, layoutmsg, mfact, -0.2"
        "$mod, J, cyclenext"
        "$mod SHIFT, J, swapnext"
        "$mod, K, cyclenext, prev"
        "$mod SHIFT, K, swapnext, prev"
        "$mod, L, layoutmsg, mfact, +0.2"

        "$mod, F, fullscreen"
        "$mod SHIFT, F, fakefullscreen"

        "$mod, Space, togglefloating, active"
        "$mod, Tab, workspace, e+1"
        "$mod SHIFT, Tab, workspace, e-1"
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
              "$mod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}"
            ]
          )
          10)
      );
  };
}
