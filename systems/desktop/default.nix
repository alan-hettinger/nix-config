{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = lib.helperFunctions.getNixFilesFromDir ./.;

  ## custom kernels that are potentially of interest for my use-case:
  ## - (linuxPackages_latest)
  ## - linuxPackages_lqx
  ## - linuxPackages_xanmod (_latest)
  ## - linuxPackages_zen
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;

  ## setting this manually here because it is not set by networking.hostName
  ## for whatever reason and I have scripts that look for it
  environment.variables = {HOSTNAME = "alan-desktop-linux";};

  system.stateVersion = "22.11";

  ## TODO split this into a separate file
  fileSystems = {
    "/mnt/media" = {
      device = "/dev/disk/by-uuid/3bee2601-fc59-43ee-82e2-02f8e0abc421";
      fsType = "ext4";
      options = ["users" "nofail" "defaults" "x-gvfs-show"];
    };
    # "/mnt/extra-ssd" = {
    #   device = "/dev/disk/by-uuid/ea13dce9-1054-476a-8394-e34a276d5bae";
    #   fsType = "ext4";
    #   options = ["users" "nofail" "defaults" "x-gvfs-show"];
    # };

    ## bind-mounting the above partitions into /home/alan/...
    "/home/alan/media" = {
      device = "/mnt/media";
      fsType = "none";
      depends = ["/mnt/media"];
      options = ["defaults" "bind"];
    };
    # "/home/alan/extra-ssd" = {
    #   device = "/mnt/extra-ssd";
    #   fsType = "none";
    #   depends = ["/mnt/extra-ssd"];
    #   options = ["defaults" "bind"];
    # };
  };

  ## TODO handle this more programatically. Enable virtualization for desktop only
  virtualisation = {
    waydroid.enable = false;
    libvirtd = {
      enable = true;
    };
    containers.enable = true;
    podman = {
      enable = true;
      dockerCompat = true;
    };
  };
  programs.virt-manager.enable = true;
}
