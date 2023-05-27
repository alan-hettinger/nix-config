{ pkgs, ... }: {

  home.packages = with pkgs; [
    mpv
    vlc
    sxiv
    libsForQt5.gwenview
    cmus
    flameshot
    krita
    discord
    betterdiscordctl
    spicetify-cli
    spotify
    libsForQt5.gwenview
    libsForQt5.tokodon # # mastodon client
    playerctl
    obs-studio
    libsForQt5.kdenlive
    scribus
    audacity
  ];

}
