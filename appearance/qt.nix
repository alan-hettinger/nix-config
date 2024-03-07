{ config, lib, pkgs, ... }: {
  qt = {
    enable = true;
    platformTheme = "qt5ct"; # gtk2 | qt5ct
    # style = "kvantum";
  };
}
