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
      substituters = [ "https://cachix.cachix.org" ];
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
    };
  };
  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 20;
      };
      efi.canTouchEfiVariables = true;
    };
    initrd.systemd.enable = true;
  };
  security = {
    sudo.enable = true;
    polkit.enable = true;
  };
  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";
  i18n = { defaultLocale = "en_US.UTF-8"; };
  console = { keyMap = "us"; };
  services = {
    printing.enable = true;
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
  };

  environment.systemPackages = with pkgs; [
    vim
    htop
    git
    wget
    xdg-utils
    polkit_gnome
    firefox
    numlockx
    coreutils
    gsmartcontrol
  ];
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };
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
