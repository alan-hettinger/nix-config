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
          greeters.slick = { enable = true; };
        };
        defaultSession = "xfce+awesome";
        sessionCommands = ''
          xset s off -dpms
        '';
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
  };
}
