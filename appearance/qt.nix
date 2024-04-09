{
  config,
  lib,
  pkgs,
  ...
}: {
  qt = {
    enable = true;
    # platformTheme = "gtk2"; # gtk2 | qt5ct
    # style = "kvantum";
  };
}
