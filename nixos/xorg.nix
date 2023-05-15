{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    xorg.xrandr
    xorg.xkill
    xorg.xwininfo
  ];
  services = {
    xserver = {
      enable = true;
      displayManager = {
        lightdm = {
          enable = true;
          greeters.slick = { enable = true; };
        };
        defaultSession = "none+awesome";
        sessionCommands = ''
          xset s off -dpms
        '';
      };
      windowManager.awesome = {
        enable = true;
        luaModules = with pkgs.luaPackages; [
          luarocks
          luarocks-nix
          luadbi-mysql
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
