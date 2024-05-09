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
