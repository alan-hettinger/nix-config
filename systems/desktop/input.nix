{
  config,
  lib,
  pkgs,
  ...
}: {
  services.libinput.mouse.middleEmulation = false;
  services.acpid.enable = true;
}
