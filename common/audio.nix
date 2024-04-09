{
  config,
  lib,
  pkgs,
  ...
}: {
  sound.enable = false; # this is for ALSA rather than pipewire
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };
}
