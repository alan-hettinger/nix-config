{
  config,
  lib,
  pkgs,
  ...
}: {
  services.xserver.videoDrivers = ["amdgpu"];
  boot.initrd.kernelModules = ["amdgpu"];
}
