{
  config,
  lib,
  pkgs,
  ...
}: {
  wayland.windowManager.hyprland.settings = {
    general.layout = "master";
    workspace = [
      "1, default:true, defaultName:web"
      "2, defaultName:code"
      "3, defaultName:notes"
      "4, defaultName:doc"
      "5, defaultName:mail"
      "6, layoutopt:orientation:center, defaultName:chat"
      "7, defaultName:files"
      "8, defaultName:media"
      "9, defaultName:game"
      "10, defaultName:"
      "f[1],gapsout:0"
      "special:emacs, on-created-empty:emacsclient -c, gapsout:0 0 0 1500, border:false, shadow:false, rounding:false"
      "special:discord, on-created-empty:discord, gapsout:0 0 0 1500, border:false, shadow:false, rounding:false"
      "special:term, on-created-empty:alacritty, gapsout:0 0 0 1500, border:false, shadow:false, rounding:false"
    ];
    master = {
      mfact = 0.55;
      new_is_master = false;
      new_on_top = false;
      no_gaps_when_only = 0;
      always_center_master = true;
      inherit_fullscreen = false;
    };
  };
}
