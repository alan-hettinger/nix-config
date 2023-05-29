{ config, lib, pkgs, ... }: {
  home.packages = with pkgs;
    [
      retroarch

    ];

}
