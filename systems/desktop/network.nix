{
  config,
  lib,
  pkgs,
  ...
}: {
  networking = {
    hostName = "alan-desktop-linux";

    networkmanager = {
      enable = true;
      dns = "default";
      settings = {
        main = {
          rc-manager = "symlink";
        };
      };
      #   ''
      #   [main]
      #   rc-manager=symlink
      # '';
    };

    wireguard.enable = true;
    iproute2.enable = true;
    resolvconf.enable = false;

    firewall = {
      enable = true;
      allowedTCPPorts = [
        22 # ssh
        53 # needed by mullvad
        80 # web
        443 # web
        1401 # needed by Mullvad
      ];
      allowedUDPPorts = [51820 53 1194 1195 1197 1300 1301 1302 1303 1400];
    };
  };

  services.mullvad-vpn = {
    enable = true;
    package =
      pkgs.mullvad-vpn; # "mullvad" is CLI only, "mullvad-vpn" is CLI and GUI
  };
}
