{ config, lib, pkgs, ... }: {
  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 20;
        consoleMode =
          "max"; # # sets the resolution of the console to highest available
        editor = false;
      };
      efi.canTouchEfiVariables = true;
    };
    initrd.systemd.enable = true;

    ## silence boot messages:
    initrd.verbose = false;
    consoleLogLevel = 0;
    kernelParams = [ "quiet" "udev.log_level=3" ];
    kernel.sysctl = { "vm.max_map_count" = 2147483642; };

    plymouth.enable = true;
  };
}
