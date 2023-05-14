{ pkgs, config, ... }: {
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

  ## This is not the "nix way" of doing things since the awesome config is "stateful" but this allows editing and reloading separately
  ## Additionally, this is necessary because the awesome config has submodules
  ## TODO manage awesome submodules using builtins.fetchGit etc
  home.activation = {
    installAwesomeConfig = ''
      if [ ! -d "${config.home.homeDirectory}/.config/awesome" ]; then
         ln -s "/home/alan/nix-config/home-manager/dotfiles/awesome" "${config.home.homeDirectory}/.config/awesome"
      fi
    '';
  };

}
