{ pkgs, ... }:

{
  home.packages = with pkgs; [
    fontconfig
    fira-code
    gyre-fonts
    dejavu_fonts
    font-awesome
    font-awesome_5
  ];

  fonts = {
    fontconfig.enable = true;

  };

}
