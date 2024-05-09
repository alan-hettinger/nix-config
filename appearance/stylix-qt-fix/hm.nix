{
  pkgs,
  config,
  lib,
  ...
}: {
  config = let
    # cfg = config.stylix.targets.qt;
    kvconfig = config.lib.stylix.colors {
      template = ./kvconfig.mustache;
      extension = ".kvconfig";
    };
    svg = config.lib.stylix.colors {
      template = ./kvantum-svg.mustache;
      extension = "svg";
    };
    kvantumPackage = pkgs.runCommandLocal "base16-kvantum" {} ''
      mkdir -p $out/share/Kvantum/Base16Kvantum
      cat ${kvconfig} >>$out/share/Kvantum/Base16Kvantum/Base16Kvantum.kvconfig
      cat ${svg} >>$out/share/Kvantum/Base16Kvantum/Base16Kvantum.svg
    '';

    qtctText = ''
      [Appearance]
      style=kvantum-dark
      icon_theme=Papirus-Dark
      standard_dialogs=gtk3

      [Fonts]
      fixed="Mononoki,14,-1,5,50,0,0,0,0,0"
      general="Montserrat,14,-1,5,50,0,0,0,0,0"
    '';
  in {
    home.packages = with pkgs; [
      qt5ct
      libsForQt5.qtstyleplugin-kvantum
      qt6Packages.qtstyleplugin-kvantum
      kvantumPackage
    ];

    qt = {
      enable = true;
      platformTheme.name = "qtct";
    };

    xdg.configFile."Kvantum/kvantum.kvconfig".source = (pkgs.formats.ini {}).generate "kvantum.kvconfig" {
      General.theme = "Base16Kvantum";
    };

    xdg.configFile."Kvantum/Base16Kvantum".source = "${kvantumPackage}/share/Kvantum/Base16Kvantum";

    xdg.configFile."qt5ct/qt5ct.conf".text = qtctText;
    xdg.configFile."qt6ct/qt6ct.conf".text = qtctText;
  };
}
