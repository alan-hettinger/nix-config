{pkgs, ...}: {
  home.packages = with pkgs; [
    go
    gore
    gotools
    gotests
    gomodifytags
    gopls
  ];
}
