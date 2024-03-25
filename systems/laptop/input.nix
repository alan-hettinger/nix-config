{ config, lib, pkgs, ... }: {
  boot.kernelParams = [ "psmouse.synaptics_intertouch=0" ];
  services = {
    xserver.libinput.enable = true;
    xserver.libinput.touchpad = {
      accelSpeed = null; # null or string
      clickMethod = "clickfinger";
      disableWhileTyping = true;
      naturalScrolling = true;
      scrollMethod = "twofinger";
      tapping = true;
      tappingButtonMap = "lrm";
      tappingDragLock = true;
    };
    acpid.enable = true;
  };

}
