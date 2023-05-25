{ pkgs, config, ... }: {
  home = {
    packages = with pkgs; [
      rofi
      i3lock-color
      xorg.xkill
      libsForQt5.ark
      # libsForQt5.kate
      libsForQt5.dolphin
      libsForQt5.dolphin-plugins
      kio-fuse
      libsForQt5.kio
      libsForQt5.kio-admin
      libsForQt5.kio-extras
      libsForQt5.qt5ct
      libsForQt5.qtstyleplugin-kvantum
      brave
      spotify
      spicetify-cli

      #gaming:
      # steam
      # xivlauncher

      gnome.gnome-font-viewer
      playerctl
      # bitwarden ## FIXME was requiring a deprecated version of nodejs

      #themes
      papirus-icon-theme
      hicolor-icon-theme
      gnome-icon-theme
      gnome.adwaita-icon-theme
      gnome.gnome-themes-extra
    ];

    ## This is not the "nix way" of doing things since the awesome config is "stateful" but this allows editing and reloading separately
    ## Additionally, this is necessary because the awesome config has git submodules
    ## TODO manage awesome submodules using builtins.fetchGit etc
    activation = {
      installAwesomeConfig = ''
        if [ ! -d "${config.home.homeDirectory}/.config/awesome" ]; then
           ln -s "/home/alan/nix-config/home-manager/dotfiles/awesome" "${config.home.homeDirectory}/.config/awesome"
        fi
      '';
    };
  };

  programs = {
    rofi = {
      enable = false;
      font = "JetbrainsMono Nerd Font 16";
      location = "center";
      terminal = "\${pkgs.alacritty}/bin/alacritty";
      plugins = with pkgs;
        [
          rofi-power-menu
          rofimoji

        ];

      extraConfig = { };
    };

  };

  xsession = {
    enable = true;
    numlock.enable = true;
    profileExtra = ''
      xmousepasteblock
      xrandr --output DisplayPort-0 --mode 2560x1440 --rate 144.00 &
    ''; # FIXME why is xmousepasteblock not running
  };

  services = {
    network-manager-applet.enable = true;
    nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
    pasystray.enable = true;

    clipmenu = {
      enable = true;
      launcher = "rofi";
    };

    redshift = {
      enable = true;
      # dawnTime = "";
      # duskTime = "";
      latitude = 33.67;
      longitude = -84.49;
      tray = true;
      temperature = {
        day = 6500;
        night = 4000;
      };
      settings = {
        # fade = 1; # FIXME
        # brightness-day = 1.0; # FIXME error about type on build
        # brightness-night = 0.8; # FIXME same
      };
    };

    picom = {
      enable = true;
      # package = pkgs.picom-jonaburg-unstable;

      activeOpacity = 1;
      backend = "glx";
      fade = true;
      fadeDelta = 20; # FIXME is this right?
      fadeSteps = [ 0.3 0.3 ]; # fade-in fade-out
      inactiveOpacity = 0.9;
      menuOpacity = 1.0;
      shadow = true;
      shadowOffsets = [ 3 3 ]; # x y
      shadowOpacity = 0.5;
      vSync = false;

      wintypes = {
        tooltip = {
          opacity = 1.0;
          shadow = false;
          full-shadow = false;
          focus = false;
          fade = false;
        };
        dock = {
          fade = false;
          shadow = false;
          opacity = 1.0;
          focus = false;
          full-shadow = false;
        };
        dnd = { shadow = false; };
        popup_menu = {
          opacity = 1.0;
          shadow = false;
          full-shadow = false;
          focus = false;
          fade = false;
        };
        dropdown_menu = {
          opacity = 1.0;
          shadow = false;
          full-shadow = false;
          focus = false;
          fade = false;
        };
      };
      shadowExclude = [
        "name = 'Notification'"
        "class_g = 'Conky'"
        "class_g ?= 'Notify-osd'"
        "class_g = 'Cairo-clock'"
        "_GTK_FRAME_EXTENTS@:c"
      ];
      fadeExclude = [ ]; # list of strings
      opacityRules = [ ];
      extraArgs =
        [ ]; # # list of strings to be passed at runtime. Example: [ "--legacy-backends" ]
      settings = {
        # for anything not included above
        inactive-opacity-override = false;
        inactive-dim = 0.1;
        corner-radius = 15;
        rounded-corners-exclude = [
          "window_type = 'dock'"
          "window_type = 'desktop'"
          ## exclude maximized windows by dimension.
          ## a consequence of this is that, on 1440p monitors,
          ## windows with a width of exactly 1920 will also be excluded
          ## this seems like a rare case.
          "widthb=2560 || widthb=1920"
        ];
        blur-method = "dual_kawase";
        blur-size = 3;
        blur-strength = 3;
        blur-kern = "3x3box";
        blur-background-exclude = [
          "window_type = 'dock'"
          "window_type = 'desktop'"
          "_GTK_FRAME_EXTENTS@:c"
        ];
        mark-wmwin-focused = true;
        mark-ovredir-focused = true;
        detect-rounded-corners = true;
        detect-client-opacity = true;
        unredir-if-possible = false;
        unredir-if-possible-exclude =
          [ "class_g = 'looking-glass-client' && !focused" ];
        glx-no-stencil = true;
        no-ewmh-fullscreen = false;
      };
    };
  };
}
