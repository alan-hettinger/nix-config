{ pkgs, ... }: {
  # basic desktop applications

  home.packages = with pkgs; [
    # note that xorg and basic wm stuff is in system config
    rofi
    picom
    i3lock-color
    xorg.xkill
    # lxappearance
    libsForQt5.ark
    # libsForQt5.kate
    libsForQt5.dolphin
    libsForQt5.dolphin-plugins
    kio-fuse
    libsForQt5.kio
    libsForQt5.kio-admin
    libsForQt5.kio-extras
    libsForQt5.qt5ct
    libsForQt5.qtstyleplugin-kvantum
    redshift
    brave
    spotify
    spicetify-cli

    #gaming:
    # steam
    # xivlauncher

    # small apps
    # xfce.xfce4-power-manager
    gnome.gnome-font-viewer
    playerctl
    bitwarden

    #themes
    papirus-icon-theme
    hicolor-icon-theme
    gnome-icon-theme
    gnome.adwaita-icon-theme
    gnome.gnome-themes-extra
  ];

  services = {
    network-manager-applet.enable = true;
    nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
    betterlockscreen.enable = false;
    pasystray.enable = true;
    # picom.enable = true;
    # picom.package = pkgs.picom-jonaburg-unstable;
  };

}
