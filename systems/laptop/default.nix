{ config, lib, pkgs, ... }: {
  imports = [
    ./laptop-configuration.nix
    ./laptop-hardware-configuration.nix
    ../common.nix
    ./input.nix
  ];
}
