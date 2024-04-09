{pkgs, ...}: {
  home.packages = with pkgs; [
    libreoffice
    pandoc
    calibre
    libsForQt5.okular
    pdfarranger
    # nextcloud-client
    thunderbird
    zotero
    onlyoffice-bin
    homebank
  ];
}
