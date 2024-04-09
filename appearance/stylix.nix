{
  pkgs,
  stylix,
  ...
}: {
  stylix = {
    autoEnable = true;
    cursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 48;
    };
    image =
      ../home-manager/dotfiles/awesome/assets/wall-mountains-catppuccin.png;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-macchiato.yaml";
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
        terminal = 16;
      };
    };

    targets = {
      plymouth.enable = true;
    };
  };

  home-manager.sharedModules = [./stylix-qt-fix/hm.nix];
  home-manager.users.alan.stylix = {
    autoEnable = true;
    targets = {
      alacritty.enable = true;
      emacs.enable = false;
      rofi.enable = false;

      gtk.extraCss = ''
        .window-frame, .window-frame:backdrop, .window-frame.solid-csd {
        box-shadow: none;
        border-style: none;
        margin: 0;
        border-radius: 0;
        border-style: hidden;
        }

        .titlebar {
        border-radius: 0;
        }

        decoration {
        box-shadow: none;
        border-style: hidden;
        margin: 0;
        border-radius: 0;
        }

        .window-frame.csd.popup {
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.2), 0 0 0 1px rgba(0, 0, 0, 0.13);
        }

        .header-bar {
        background-image: none;
        box-shadow: none;
        }

        window.background { border-radius: 0; }
      '';
    };
  };
}
