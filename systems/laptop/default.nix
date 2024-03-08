{ config, lib, pkgs, ... }: {
  imports = lib.helperFunctions.getNixFilesFromDir ./.;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  ## setting this manually here because it is not set by networking.hostName
  ## for whatever reason and I have scripts that look for it
  environment.variables = { HOSTNAME = "alan-laptop-linux"; };

  system.stateVersion = "22.11";
}
