{ config, lib, pkgs, ... }: {
  imports = [
    ./laptop-hardware-configuration.nix
    ../common.nix
    ./input.nix
    ./network.nix
    ./display.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  ## setting this manually here because it is not set by networking.hostName
  ## for whatever reason and I have scripts that look for it
  environment.variables = { HOSTNAME = "alan-laptop-linux"; };

  system.stateVersion = "22.11";
}
