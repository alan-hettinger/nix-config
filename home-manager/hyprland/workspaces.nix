{
  config,
  lib,
  pkgs,
  ...
}: let
  specialWorkspaceGaps = "gapsout:0 0 0 1500";
  specialWorkspaceDecoration = "border:false, shadow:false, rounding:false";
  specialWorkspaceProps = "${specialWorkspaceGaps}, ${specialWorkspaceDecoration}";
in {
  wayland.windowManager.hyprland.settings = {
    general.layout = "master";
    workspace = [
      "1, default:true, defaultName:web"
      "2, defaultName:code"
      "3, defaultName:notes"
      "4, defaultName:doc"
      "5, defaultName:book"
      "6, defaultName:chat, layoutopt:orientation:center"
      "7, defaultName:files"
      "8, defaultName:media"
      "9, defaultName:game"
      "10, defaultName:misc"
      "f[1],gapsout:0, border:false"
      "special:emacs, on-created-empty:emacsclient -c, ${specialWorkspaceProps}"
      "special:discord, on-created-empty:discord, ${specialWorkspaceProps}"
      "special:term, on-created-empty:alacritty, ${specialWorkspaceProps}"
      # "special:file-manager, on-created-empty:dolphin, ${specialWorkspaceProps}"
    ];
    master = {
      mfact = 0.55;
      new_status = "slave";
      new_on_top = false;
      always_center_master = true;
      inherit_fullscreen = false;
    };
    # windowrulev2 = [
    #   "workspace +0, onworkspace:special:file-manager, title:^(?!.*Dolphin).*"
    # ];
  };
}
