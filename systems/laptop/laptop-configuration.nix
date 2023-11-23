{ inputs, pkgs, ... }: {
  nix = {
    gc = {
      dates = "weekly";
      automatic = true;
      options = "--delete-older-than 30d";
    };
    package = pkgs.nixVersions.unstable;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    initrd.kernelModules = [ "amdgpu" ];
    kernel.sysctl = { "vm.max_map_count" = 2147483642; };
  };

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

  services.acpid.enable =
    true; # saw this in someone's config TODO double check if needed

  services.tlp = { # power saving settings
    enable = true;
    settings = {
      # GPU power management method:
      RADEON_DPM_STATE_ON_AC = "performance";
      RADEON_DPM_STATE_ON_BAT = "battery";

      # power/performance levels, thermals, fan speed:
      PLATFORM_PROFILE_ON_AC = "performance";
      PLATFORM_PROFILE_ON_BAT = "low-power";

      # automatic frequency scaling
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      # cpu "turbo core" - 0=disable 1=enable
      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;

      # Minimize number of cores - 0=disable 1=enable
      SCHED_POWERSAVE_ON_AC = 0;
      SCHED_POWERSAVE_ON_BAT = 1;

      # self-explanatory
      DEVICES_TO_DIABLE_ON_STARTUP = "bluetooth";

      # runtime power management for PCIe devices:
      RUNTIME_PM_ON_AC = "on"; # devices powered on constantly
      RUNTIME_PM_ON_BAT = "auto";

    };
  };

  services.xserver.serverFlagsSection = ''
    Option "BlankTime" "20"
    Option "StandbyTime" "30"
    Option "SuspendTime" "60"
    Option "OffTime" "0"
  '';

  services.xserver.videoDrivers = [ "amdgpu" ];

  services.autorandr = {
    enable = false;
    profiles = {
      "clamshell" = {

      };
      "dual" = {

      };
      "laptop" = {

      };
      "mirror" = {

      };
    };
  };

  services.udisks2.enable = true;

  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  security.rtkit.enable = true;

  qt = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };
  environment = {
    pathsToLink = [ "/share/zsh" ];
    ## setting this manually here because it is not set by networking.hostName
    ## for whatever reason and I have scripts that look for it
    variables = { HOSTNAME = "alan-laptop-linux"; };
  };

  system.stateVersion = "22.11";
}
