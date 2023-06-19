{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    xorg.xrandr
    xorg.xkill
    xorg.xwininfo
  ];
  services = {
    xserver = {
      enable = true;
      desktopManager = {
        xterm.enable = false;
        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
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
      layout = "us";
      xkbOptions = "caps:escape";
    };
    redshift.enable = true;

    # required by thunar:
    gvfs.enable = true;
    tumbler.enable = true;

  };
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [
      thunar-volman
      thunar-archive-plugin
    ];
  };
}
