{ pkgs, config, ... }: {
  home = {
    packages = with pkgs; [
      i3lock-color
      xorg.xkill
      xarchiver
      libsForQt5.ark
      libsForQt5.dolphin
      libsForQt5.dolphin-plugins
      kio-fuse
      libsForQt5.kio
      libsForQt5.kio-admin
      libsForQt5.kio-extras
      libsForQt5.qtstyleplugins
      # libsForQt5.qt5ct

      brave
      spotify
      spicetify-cli
      cinnamon.nemo-with-extensions

      rofi-power-menu # FIXME should be installed by rofi plugins but isnt

      gnome.gnome-font-viewer
      playerctl
      bitwarden

      #themes
      papirus-icon-theme
      hicolor-icon-theme
      gnome-icon-theme
      gnome.adwaita-icon-theme
      gnome.gnome-themes-extra
    ];

    # install awesome config and dependencies (currently lain is the only dep)
    file = {
      ".config/awesome/lain".source = pkgs.fetchFromGitHub {
        owner = "lcpz";
        repo = "lain";
        rev = "88f5a8abd2649b348ffec433a24a263b37f122c0";
        sha256 = "MH/aiYfcO3lrcuNbnIu4QHqPq25LwzTprOhEJUJBJ7I=";
      };
      ".config/awesome" = {
        source = ./dotfiles/awesome;
        recursive = true;
      };
    };
  };

  programs = {
    rofi = {
      enable = true;
      # font = "JetbrainsMono Nerd Font 16";
      location = "center";
      terminal = "alacritty";
      plugins = with pkgs; [
        rofi-power-menu # # FIXME not available on path as expected
        rofimoji

      ];
      extraConfig = {
        icon-theme = "Papirus-Dark";
        show-icons = true;
        modi = "drun,window,filebrowser,combi";
        combi-modes = [ "window" "drun" ];
        combi-hide-mode-prefix = true;

        ## keybindings:
        kb-mode-next = "Shift+Right,Tab,Control+l";
        kb-mode-previous = "Shift+Left,Control+h";
        kb-element-next = "Control+j";
        kb-element-prev = "Control+k";

        ## unbind to make room for the above:
        kb-remove-to-eol = "";
        kb-mode-complete = "";
        kb-remove-char-back = "BackSpace,Shift+BackSpace";
        kb-accept-entry = "Return,KP_Enter";
      };
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
