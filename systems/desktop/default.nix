{ config, lib, pkgs, ... }:

{
  imports = [
    ./desktop-configuration.nix
    ./desktop-hardware-configuration.nix
    ../common.nix
  ];
}
