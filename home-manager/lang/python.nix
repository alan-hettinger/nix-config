{ config, lib, pkgs, ... }: {

  home.packages = with pkgs; [
    python310Full
    python310Packages.pytest
    python310Packages.nose
    python310Packages.black
    python310Packages.pyls-spyder

  ];

}
