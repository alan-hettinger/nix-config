{ config, lib, pkgs, ... }: {

  networking = {
    hostName = "alan-laptop-linux";

    firewall = {
      enable = false;
      allowedTCPPorts = [
        22 # ssh
        80 # web
        443 # web
      ];
    };
  };

  services.mullvad-vpn = {
    enable = true;
    package =
      pkgs.mullvad-vpn; # "mullvad" is CLI only, "mullvad-vpn" is CLI and GUI
  };

}
