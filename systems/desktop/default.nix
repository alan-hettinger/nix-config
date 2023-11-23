{ config, lib, pkgs, ... }: {
  imports = [
    ./desktop-hardware-configuration.nix
    ../common.nix
    ./network.nix

  ];

  ## custom kernels that are potentially of interest for my use-case:
  ## - (linuxPackages_latest)
  ## - linuxPackages_lqx
  ## - linuxPackages_xanmod (_latest)
  ## - linuxPackages_zen
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;

  ## setting this manually here because it is not set by networking.hostName
  ## for whatever reason and I have scripts that look for it
  environment.variables = { HOSTNAME = "alan-desktop-linux"; };

  system.stateVersion = "22.11";
}
