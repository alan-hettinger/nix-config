{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  services.udisks2.enable = true;

  services.printing = {
    enable = true;
    drivers = with pkgs; [
      # drivers for my brother printer:
      # cups-brother-hll2340dw
      brlaser
    ];
  };
  services.avahi = {
    # needed for printing over wifi
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
  services.system-config-printer.enable = true;

  services.blueman.enable = true;

  services.flatpak.enable = true;
  services.accounts-daemon.enable = true;

  services.gnome.gnome-keyring.enable = true;
  services.fwupd = {enable = true;};

  security = {
    sudo.enable = true;
    sudo.extraConfig = "Defaults pwfeedback";
    polkit.enable = true;
    rtkit.enable = true;
  };
  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";
  i18n = {defaultLocale = "en_US.UTF-8";};
  console = {
    # keyMap = "us";
    earlySetup = true;
    useXkbConfig = true;
  };
  hardware = {
    bluetooth = {
      enable = true;
      settings = {General = {Experimental = true;};};
    };
    pulseaudio.enable = false;
  };
  programs = {
    zsh.enable = true;
    dconf.enable = true;
    system-config-printer.enable = true;
  };
  environment = {
    pathsToLink = ["/share/zsh"];
    systemPackages = with pkgs; [
      vim
      htop
      git
      wget
      xdg-utils
      polkit_gnome
      firefox
      numlockx
      coreutils
      gparted
      gnome.gnome-disk-utility
    ];
  };
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
  };

  # location stuff:
  location = {
    provider = "manual";
    latitude = 33.7;
    longitude = -84.3;
  };
  # services.geoclue2 = {
  #   enable = true;
  #   enableWifi = true;
  #   appConfig.redshift.isAllowed = true;
  # };

  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = ["graphical-session.target"];
      wants = ["graphical-session.target"];
      after = ["graphical-session.target"];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };
  users = {
    defaultUserShell = pkgs.zsh;
    users.alan = {
      isNormalUser = true;
      description = "alan";
      initialPassword = "";
      extraGroups = ["networkmanager" "wheel" "video" "audio"];
    };
  };
}
