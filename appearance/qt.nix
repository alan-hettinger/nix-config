{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = [pkgs.libsForQt5.qt5.qtwayland];
  qt = {
    enable = true;
    # platformTheme = "gtk2"; # gtk2 | qt5ct
    # style = "kvantum";
  };
}
