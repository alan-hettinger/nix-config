{ config, lib, pkgs, ... }: {
  imports = [
    ./stylix.nix
    ./font.nix
    ./qt.nix

  ];
}
