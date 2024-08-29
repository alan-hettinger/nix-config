{pkgs, ...}: {
  home.packages = with pkgs; [
    libreoffice
    pandoc
    # calibre
    foliate
    libsForQt5.okular
    pdfarranger
    thunderbird
    zotero
    onlyoffice-bin
    homebank
  ];
}
