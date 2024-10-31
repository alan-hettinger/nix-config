{
  config,
  lib,
  pkgs,
  ...
}: let
  browser = "firefox";
  terminal = "alacritty -e tmux attach";
  launcher = "pkill rofi || rofi -show combi -location 1 -display-combi '>>'";
  windowswitcher = "rofi -show window -sidebar-mode";
  editor = "emacsclient -c";
  screenshotPath = "~/Pictures/screenshots";
  screenshot = "wayshot -s \"$(slurp)\" -f ${screenshotPath}/$(date --iso-8601=seconds).jpg";
  screenshotFull = "wayshot -f ${screenshotPath}/$(date --iso-8601=seconds).jpg";
  filemanager = "dolphin";
  browser2 = "brave";
  # powerMenu = "rofi -show power-menu -modi power-menu:rofi-power-menu -location 3";
  powerMenu = "pkill wlogout || wlogout";
  # clipboard = "clipman pick -t rofi";
  clipboard = "pkill rofi || cliphist list | rofi -dmenu | cliphist decode | wl-copy";
  # lock = "hyprlock";
  volumeUpCmd = "swayosd-client --output-volume raise";
  volumeDownCmd = "swayosd-client --output-volume lower";
  volumeMuteCmd = "swayosd-client --output-volume mute-toggle";
  inputMuteCmd = "swayosd-client --input-volume mute-toggle";

  brightnessUpCmd = "swayosd-client --brightness raise";
  brightnessDownCmd = "swayosd-client --brightness lower";
in {
  home.packages = with pkgs; [
    killall
    libnotify
  ];
  wayland.windowManager.hyprland = {
    settings = {
      "$mod" = "Super_L";

      binds = [
        ", Super_R, sendshortcut, Hyper_R"
      ];

      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];

      # passes to window in addition to dispatching
      bindn = [
        ", mouse:274, exec, wl-copy -p ''"
      ];

      # repeats when held
      binde = [
        "$mod, H, layoutmsg, mfact -0.05"
        "$mod, L, layoutmsg, mfact +0.05"
        ", XF86MonBrightnessUp, exec, ${brightnessUpCmd}"
        ", XF86MonBrightnessDown, exec, ${brightnessDownCmd}"
        ", XF86AudioLowerVolume, exec, ${volumeDownCmd}"
        ", XF86AudioRaiseVolume, exec, ${volumeUpCmd}"
        ", XF86AudioMute, exec, ${volumeMuteCmd}"
        ", XF86AudioMicMute, exec, ${inputMuteCmd}"
      ];

      bind =
        [
          "$mod, O, togglespecialworkspace, emacs"
          "$mod, C, togglespecialworkspace, discord"
          "$mod, Return, togglespecialworkspace, term"

          "$mod, BackSpace, movetoworkspace, +0"

          "$mod, X, exec, [workspace 1] ${browser}"
          "$mod SHIFT, X, exec, [workspace 1] ${browser2}"
          "$mod SHIFT, Return, exec, ${terminal}"

          "$mod, Z, togglespecialworkspace, file-manager"
          "$mod SHIFT, Z, exec, ${filemanager}"

          "$mod, V, exec, ${editor}"
          "$mod, D, exec, ${launcher}"
          "$mod, P, exec, ${screenshot}"
          "$mod SHIFT, P, exec, ${screenshotFull}"
          "$mod, W, exec, ${windowswitcher}"
          "$mod, Escape, exec, ${powerMenu}"
          "$mod, comma, exec, ${clipboard}"
          "$mod, grave, exec, hyprlock"

          "$mod, F1, exec, ~/.local/share/scripts/wayland/hypr-game-mode.sh"

          "$mod, Q, killactive"

          "$mod, J, cyclenext"
          "$mod SHIFT, J, swapnext"
          "$mod, K, cyclenext, prev"
          "$mod SHIFT, K, swapnext, prev"

          "$mod, F, fullscreen"
          # "$mod SHIFT, F, fakefullscreen" # FIXME broken after update

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

          "$mod, M, fullscreen, 1"

          "$mod, backslash, layoutmsg, orientationcycle left center"

          "$mod, B, exec, pkill -SIGUSR1 waybar"
          # TODO mod,= reduces gap size
          # TODO mod,- increases gap size
          # TODO mod,CTRL,j|k cycles screens
          # TODO mod,CTRL,Enter launches default apps
          # TODO mod,/ toggles tray visibility
          # TODO mod,F10|F11|F12 control volume
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

    # defining submaps in a string to make sure they're grouped together
    extraConfig = ''
      bind=$mod, Space, submap, keychords-main

      submap=keychords-main
      bind=,escape, submap, reset
      bindr=$mod, SUPER_L, submap, reset
      bind=, W, submap, window
      submap=reset

      submap=window
      bind=,escape, submap, reset
      bindr=$mod, SUPER_L, submap, reset
      bind=,1, movetoworkspacesilent, 1
      bind=,2, movetoworkspacesilent, 2
      bind=,3, movetoworkspacesilent, 3
      bind=,4, movetoworkspacesilent, 4
      bind=,5, movetoworkspacesilent, 5
      bind=,6, movetoworkspacesilent, 6
      bind=,7, movetoworkspacesilent, 7
      bind=,8, movetoworkspacesilent, 8
      bind=,9, movetoworkspacesilent, 9
      bind=,0, movetoworkspacesilent, 10
      bind=$mod,1, workspace, 1
      bind=$mod,2, workspace, 2
      bind=$mod,3, workspace, 3
      bind=$mod,4, workspace, 4
      bind=$mod,5, workspace, 5
      bind=$mod,6, workspace, 6
      bind=$mod,7, workspace, 7
      bind=$mod,8, workspace, 8
      bind=$mod,9, workspace, 9
      bind=$mod,0, workspace, 10
      bind=SHIFT,1, movetoworkspace, 1
      bind=SHIFT,2, movetoworkspace, 2
      bind=SHIFT,3, movetoworkspace, 3
      bind=SHIFT,4, movetoworkspace, 4
      bind=SHIFT,5, movetoworkspace, 5
      bind=SHIFT,6, movetoworkspace, 6
      bind=SHIFT,7, movetoworkspace, 7
      bind=SHIFT,8, movetoworkspace, 8
      bind=SHIFT,9, movetoworkspace, 9
      bind=SHIFT,0, movetoworkspace, 10
      submap=reset

    '';
  };
}
