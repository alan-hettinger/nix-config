{ inputs, lib, config, pkgs, ... }: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-amd
    # inputs.hardware.nixosModules.common-ssd

    ./hardware-configuration.nix
    ./font.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = { allowUnfree = true; };
  };

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
      # substituters = [ "https://cachix.cachix.org" ];
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
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = pkgs.linuxPackages_latest;
    initrd = {
      systemd.enable = true;
      kernelModules = [ "amdgpu" ];
    };
  };

  boot.kernel.sysctl = { "vm.max_map_count" = 2147483642; };

  security.sudo.enable = true;

  networking = {
    hostName = "alan-desktop-linux";
    networkmanager.enable = true;

    firewall = {
      enable = false;
      allowedTCPPorts = [
        22 # ssh
        80 # web
        443 # web
      ];
    };
  };

  time.timeZone = "America/New_York";
  i18n = { defaultLocale = "en_US.UTF-8"; };
  console = { keyMap = "us"; };

  services.xserver = {
    enable = true;
    displayManager = {
      lightdm = {
        enable = true;
        greeters.slick = {
          enable = true;

        };
      };
      defaultSession = "none+awesome";
      sessionCommands = ''
        xset s off -dpms
      '';
    };
    windowManager.awesome = {
      enable = true;
      luaModules = with pkgs.luaPackages; [
        luarocks
        luarocks-nix
        luadbi-mysql
        luaposix
        lgi
        vicious
      ];
    };
    serverFlagsSection = ''
      Option "BlankTime" "0"
      Option "StandbyTime" "0"
      Option "SuspendTime" "0"
      Option "OffTime" "0"
    '';

    videoDrivers = [ "amdgpu" ];
    xrandrHeads = [
      "DisplayPort-0"
      {
        output = "DisplayPort-0";
        primary = true;
        monitorConfig = ''
          Option "DPMS" "false"
          Option "PreferredMode" "2560x1440_144.00"
        '';
      }
    ];
    ## FIXME:
    # deviceSection = ''
    #   Identifier "AMD"
    #   Option "VariableRefresh" "true"
    # '';
    layout = "us";
    xkbOptions = "caps:escape"; # change caps lock to escape
  };

  environment.extraInit = ''
    xset s off -dpms
  '';

  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  hardware.bluetooth = {
    enable = true;
    settings = { General = { Experimental = true; }; };
  };

  services.printing.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  programs.zsh.enable = true;

  users.users.alan = {
    isNormalUser = true;
    description = "alan";
    initialPassword = "";
    extraGroups = [ "networkmanager" "wheel" "video" "audio" ];
  };

  qt = {
    enable = true;
    platformTheme = "qt5ct";
    style = "adwaita-dark";
  };

  environment.systemPackages = with pkgs; [
    vim
    htop
    git
    gh
    wget
    xdg-utils
    borgbackup
    borgmatic
    polkit_gnome
    xorg.xkill
    firefox
    numlockx
    coreutils
    xorg.xrandr
    xorg.xwininfo
  ];

  environment.pathsToLink = [ "/share/zsh" ];

  ## setting this manually here because it is not set by networking.hostName
  ## for whatever reason and I have scripts that look for it
  environment.variables = { HOSTNAME = "alan-desktop-linux"; };

  services.flatpak.enable = true;
  services.accounts-daemon.enable = true;
  xdg.portal.enable = true;
  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  programs.dconf.enable = true;

  services.gnome.gnome-keyring.enable = true;
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

  services.acpid.enable =
    true; # saw this in someone's config TODO double check if needed
  security.polkit.enable = true;
  services.upower.enable = false;
  services.tlp.enable = false;

  services.udisks2.enable = true;

  services.redshift.enable = true;
  location = {
    provider = "manual";
    latitude = 33.753;
    longitude = -84.386;
  };

  users.defaultUserShell = pkgs.zsh;
  system.stateVersion = "22.11";
}
