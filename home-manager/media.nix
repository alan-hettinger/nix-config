{ pkgs, ... }: {

  home.packages = with pkgs; [
    mpv
    vlc
    libmicrodns # DNS resolver library used by vlc
    protobuf # used by vlc for streaming to chromecast
    sxiv
    cmus
    flameshot
    krita
    discord
    betterdiscordctl
    spicetify-cli
    spotify
    # libsForQt5.gwenview
    # libsForQt5.tokodon # # mastodon client
    playerctl
    obs-studio
    libsForQt5.kdenlive
    scribus
    audacity
  ];

}
