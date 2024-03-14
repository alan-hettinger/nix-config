{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    xorg.xrandr
    xorg.xkill
    xorg.xwininfo
  ];

  services = {
    xserver = {
      excludePackages = with pkgs; [ xterm ];
      enable = true;
      desktopManager = {
        xterm.enable = false;
        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
          enableScreensaver = true;
        };
      };
      displayManager = {
        lightdm = {
          enable = true;
          greeters.slick = {
            enable = true;
            theme = {
              package = pkgs.catppuccin-gtk.override {
                accents = [ "rosewater" ];
                size = "compact";
                variant = "macchiato";
              };
              name = "Catppuccin-Macchiato-Compact-Rosewater-Dark";
            };
            font = {
              package = pkgs.source-sans-pro;
              name = "Source Sans Pro 16";
            };
            cursorTheme = {
              size = 48;
              package = pkgs.vanilla-dmz;
              name = "Vanilla-DMZ";
            };
          };
        };
        defaultSession = "xfce+awesome";
        sessionCommands = ''
          xset s off -dpms
        ''; # TODO is this necessary with xfce handling displays
      };
      windowManager.awesome = {
        enable = true;
        luaModules = with pkgs.luaPackages; [
          luarocks
          luarocks-nix
          # luadbi-mysql # FIXME was broken on update but what does this do
          luaposix
          lgi
          vicious
        ];
      };
      xkb = {
        options = "caps:escape";
        layout = "us";
      };
    };
    redshift = {
      enable = true;
      executable = "/bin/redshift-gtk";
      temperature = {
        day = 6500;
        night = 4000;
      };
      brightness = {
        day = "1.0";
        night = "0.7";
      };
      extraOptions = [ "-m randr:screen=0" ];
    };

    # required by thunar:
    gvfs.enable = true;
    tumbler.enable = true;

  };
  environment.xfce = {
    excludePackages = with pkgs.xfce; [
      mousepad
      xfce4-appfinder
      # xfce4-screenshooter
      xfce4-taskmanager
      xfce4-terminal
      xfce4-notifyd
      parole

      ## xfce apps that I do want:
      # ristretto
      # xfce4-settings
      # xfce.thunar
    ];
  };
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [ thunar-volman thunar-archive-plugin ];
  };
}
