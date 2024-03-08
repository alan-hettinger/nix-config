{ config, lib, pkgs, ... }: {
  boot.initrd.kernelModules = [ "amdgpu" ];
  services.xserver.videoDrivers = [ "amdgpu" ];
  services.xserver.serverFlagsSection = ''
    Option "BlankTime" "0"
    Option "StandbyTime" "0"
    Option "SuspendTime" "0"
    Option "OffTime" "0"
  '';
  services.xserver.xrandrHeads = [
    "DisplayPort-0"
    {
      output = "DisplayPort-0";
      primary = true;
      monitorConfig = ''
        Option "DPMS" "false"
        Option "PreferredMode" "2560x1440_144.00"
      '';
    }
    "HDMI-A-0"
    {
      output = "HDMI-A-0";
      primary = false;
      monitorConfig = ''
        Option "DPMS" "false"
        Option "RightOf" "DisplayPort-0"
      '';
    }
  ];
  services.xserver.deviceSection = ''Option "VariableRefresh" "True"'';
  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };
  environment.extraInit = ''
    xset s off -dpms
  '';
}
