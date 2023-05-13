{ pkgs, ... }:

{
  home.packages = with pkgs; [
    go
    gore
    gotools
    gocode
    gotests
    gomodifytags
    gopls
  ];

}
