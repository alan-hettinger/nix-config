{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.anyrun = {
    enable = true;
  };
}
