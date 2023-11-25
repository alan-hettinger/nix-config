{ config, lib, pkgs, ... }: {

  services.xserver.libinput.mouse.middleEmulation = false;
  services.acpid.enable = true;
}
