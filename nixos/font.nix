{ pkgs, config, ... }:

{
  console.font = "ter-m32n";
  fonts = {
    fontDir.enable = true;
    fontconfig = {
      enable = true;
      defaultFonts.monospace = [ "jetbrains-mono" ];
      defaultFonts.sansSerif = [ "source-sans-pro" ];
      defaultFonts.serif = [ "source-serif-pro" ];
    };
    fonts = with pkgs; [
      gyre-fonts
      dejavu_fonts
      font-awesome
      font-awesome_5
      source-code-pro
      source-serif-pro
      source-sans-pro
      liberation_ttf
      dejavu_fonts
      (nerdfonts.override { fonts = [ "FiraCode" "JetBrainsMono" "Iosevka" ]; })
    ];

  };

}
