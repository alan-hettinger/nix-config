{ pkgs, ... }: {
  console.font = "ter-m32n";
  fonts = {
    fontDir.enable = true;
    fontconfig = {
      enable = true;
      defaultFonts.monospace = [ "jetbrains-mono" ];
      defaultFonts.sansSerif = [ "source-sans-pro" ];
      defaultFonts.serif = [ "source-serif-pro" ];
    };
    packages = with pkgs; [
      gyre-fonts
      dejavu_fonts
      font-awesome
      font-awesome_5
      source-code-pro
      source-serif-pro
      source-sans-pro
      liberation_ttf
      dejavu_fonts
      libre-baskerville
      vollkorn
      montserrat
      open-sans
      mononoki
      # symbola # possibly a fix to some doom emacs issues with all-the-icons; though modeline readme claims this is usually a windows problem
      # emacs-all-the-icons-fonts
      (nerdfonts.override { fonts = [ "FiraCode" "JetBrainsMono" "Iosevka" "NerdFontsSymbolsOnly" "Mononoki" ]; })
    ];
  };
}
