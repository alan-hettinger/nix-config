{ config, lib, pkgs, ... }: {
  qt = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };
}
