{ inputs, pkgs, ... }: {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    ./desktop-hardware-configuration.nix
    ./font.nix
    ./common.nix
    ./xorg.nix
  ];

  ## import home-manager so we can rebuild the whole system at once:
  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    users = { alan = import ../home-manager/home.nix; };
  };

  nixpkgs.overlays = [
    # (final: prev: {
    # })
  ];

  nix = {
    gc = {
      dates = "weekly";
      automatic = true;
      options = "--delete-older-than 30d";
    };
    package = pkgs.nixVersions.unstable;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    initrd.kernelModules = [ "amdgpu" ];
    kernel.sysctl = { "vm.max_map_count" = 2147483642; };
  };

  networking = {
    hostName = "alan-desktop-linux";

    firewall = {
      enable = false;
      allowedTCPPorts = [
        22 # ssh
        80 # web
        443 # web
      ];
    };
  };
  services = {
    xserver = {

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
        "HDMI-A-0"
        {
          output = "HDMI-A-0";
          primary = false;
          monitorConfig = ''
            Option "Rotate" "left"
            Option "DPMS" "false"
            Option "RightOf" "DisplayPort-0"
          '';
        }
      ];
      deviceSection = ''Option "VariableRefresh" "True"'';

      libinput.mouse.middleEmulation = false;
    };

    mullvad-vpn = {
      enable = true;
      package =
        pkgs.mullvad-vpn; # "mullvad" is CLI only, "mullvad-vpn" is CLI and GUI
    };

    acpid.enable =
      true; # saw this in someone's config TODO double check if needed
    # upower.enable = false;
    tlp.enable = false;
    udisks2.enable = true;
  };

  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  security.rtkit.enable = true;

  qt = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };
  environment = {
    extraInit = ''
      xset s off -dpms
    '';
    systemPackages = with pkgs; [
      borgbackup
      borgmatic

    ];
    pathsToLink = [ "/share/zsh" ];
    ## setting this manually here because it is not set by networking.hostName
    ## for whatever reason and I have scripts that look for it
    variables = { HOSTNAME = "alan-desktop-linux"; };
  };

  programs.gamemode.enable = true;

  location = {
    provider = "manual";
    latitude = 33.753;
    longitude = -84.386;
  };

  system.stateVersion = "22.11";
}
