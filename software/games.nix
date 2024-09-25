{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.gamemode.enable = true;
  # hardware.xone.enable = true; # FIXME build failure
}
