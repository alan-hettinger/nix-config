{
  config,
  lib,
  pkgs,
  ...
}: {
  services = {
    tlp = {
      # power saving settings
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

        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "power";

        # cpu "turbo core" - 0=disable 1=enable
        CPU_BOOST_ON_AC = 1;
        CPU_BOOST_ON_BAT = 0;

        # Minimize number of cores - 0=disable 1=enable
        SCHED_POWERSAVE_ON_AC = 0;
        SCHED_POWERSAVE_ON_BAT = 1;

        # self-explanatory
        DEVICES_TO_DIABLE_ON_STARTUP = "bluetooth";

        # runtime power management for PCIe devices:
        RUNTIME_PM_ON_AC = "auto";
        RUNTIME_PM_ON_BAT = "auto";

        # wifi power saving mode
        # off = performance; on = save power
        WIFI_PWR_ON_AC = "off";
        WIFI_PWR_ON_BAT = "off"; # power saving causes unstable connection

        # do not autosuspend bluetooth USB devices
        USB_EXCLUDE_BTUSB = 1;
      };
    };

    xserver.serverFlagsSection = ''
      Option "BlankTime" "20"
      Option "StandbyTime" "30"
      Option "SuspendTime" "60"
      Option "OffTime" "0"
    '';
  };
}
