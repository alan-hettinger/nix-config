{ inputs, config, lib, pkgs, ... }: {

  nixpkgs.config = { allowUnfree = true; };
  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
      config.nix.registry;

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      substituters =
        [ "https://cachix.cachix.org" "https://hyprland.cachix.org" ];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      ];
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
    };

    gc = {
      dates = "weekly";
      automatic = true;
      options = "--delete-older-than 30d";
    };
    package = pkgs.nixVersions.unstable;
  };
  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 20;
        consoleMode =
          "max"; # # sets the resolution of the console to highest available
        editor = false;
      };
      efi.canTouchEfiVariables = true;
    };
    initrd.systemd.enable = true;

    ## silence boot messages:
    initrd.verbose = false;
    consoleLogLevel = 0;
    kernelParams = [ "quiet" "udev.log_level=3" ];
    kernel.sysctl = { "vm.max_map_count" = 2147483642; };

    plymouth.enable = true;
  };
  services = {
    udisks2.enable = true;

    printing = {
      enable = true;
      drivers = with pkgs;
        [
          # drivers for my brother printer:
          cups-brother-hll2340dw

        ];
    };
    avahi = { # needed for printing over wifi
      enable = true;
      nssmdns = true;
      openFirewall = true;
    };
    system-config-printer.enable = true;
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

    blueman.enable = true;

    flatpak.enable = true;
    accounts-daemon.enable = true;

    gnome.gnome-keyring.enable = true;
    fwupd = { enable = true; };

  };
  security = {

    sudo.enable = true;
    polkit.enable = true;

    rtkit.enable = true;
  };
  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";
  i18n = { defaultLocale = "en_US.UTF-8"; };
  console = {
    # keyMap = "us";
    earlySetup = true;
    useXkbConfig = true;
  };
  sound.enable = true;
  hardware = {
    bluetooth = {
      enable = true;
      settings = { General = { Experimental = true; }; };
    };
    pulseaudio.enable = false;
  };
  programs = {
    zsh.enable = true;
    dconf.enable = true;
    system-config-printer.enable = true;
  };
  environment = {

    pathsToLink = [ "/share/zsh" ];
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
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
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
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart =
          "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
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
      extraGroups = [ "networkmanager" "wheel" "video" "audio" ];
    };
  };

}