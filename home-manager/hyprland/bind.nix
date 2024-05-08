{
  config,
  lib,
  pkgs,
  ...
}: let
  browser = "firefox";
  terminal = "alacritty";
  launcher = ''    rofi -show combi \
                        -location 1 \
                        -display-combi '>>' \
                        -theme-str '#window { y-offset: 45px; }' '';
  windowswitcher = "rofi -show window -sidebar-mode";
  editor = "emacsclient -c";
  screenshot = "flameshot gui";
  filemanager = "dolphin";
  browser2 = "brave";
in {
  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";
    bind =
      [
        "$mod, X, exec, ${browser}"
        "$mod SHIFT, X, exec ${browser2}"
        "$mod, Return, exec, ${terminal}"
        "$mod, Z, exec, ${filemanager}"
        "$mod, C, exec, discord"
        "$mod, V, exec, ${editor}"
        "$mod, D, exec, ${launcher}"
        "$mod, P, exec, ${screenshot}"
        "$mod, W, exec, ${windowswitcher}"
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
              "$mod SHIFT ${ws}, movetoworkspace, ${toString (x + 1)}"
            ]
          )
          10)
      );
  };
}
