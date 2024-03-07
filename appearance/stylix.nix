{ pkgs, stylix, ... }: {

  stylix = {
    autoEnable = true;
    cursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 48;
    };
    image =
      ../home-manager/dotfiles/awesome/assets/wall-mountains-catppuccin.png;
    base16Scheme =
      "${pkgs.base16-schemes}/share/themes/catppuccin-macchiato.yaml";
    polarity = "dark";
    fonts = {
      serif = {
        package = pkgs.source-serif-pro;
        name = "Source Serif Pro";
      };
      sansSerif = {
        package = pkgs.montserrat;
        name = "Montserrat";
      };
      monospace = {
        package = pkgs.mononoki;
        name = "mononoki";
      };
      sizes = {
        applications = 16;
        desktop = 16;
        popups = 16;
        terminal = 14;
      };
    };

    targets = {
      plymouth = {
        enable = true;
        blackBackground = false;
      };
    };
  };

  home-manager.users.alan.stylix = {
    autoEnable = true;
    targets = {
      alacritty.enable = true;
      emacs.enable = false;
      rofi.enable = false;
      kde.enable = false;

      firefox = {
        enable = true;
        profileNames = [ "alan" ];
      };

      gtk.extraCss = ''
        .window-frame { box-shadow: 0 0 0 0px @wm_border; margin: 0px; }
      '';
    };
  };
}
