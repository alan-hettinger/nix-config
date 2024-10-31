{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    mangohud
  ];
  programs.gamemode = {
    enable = true;
    settings = {
      general = {
        softrealtime = "auto"; # uses realtime scheduler when available
        renice = 10; # Gives games higher priority. Reverse of typical niceness settings, higher number is higher priority.
        inhibit_screensaver = 0; # I don't have a screensaver
      };
    };
  };
  programs.steam = {
    enable = true;
    extraPackages = with pkgs; [
      gamemode

      # # xorg libraries, might be needed to run gamescope
      xorg.libXcursor
      xorg.libXi
      xorg.libXinerama
      xorg.libXScrnSaver
      libpng
      libpulseaudio
      libvorbis
      stdenv.cc.cc.lib
      libkrb5
      keyutils
    ];

    # run a gamescope session from display manager:
    gamescopeSession = {
      enable = true;
    };
  };

  programs.gamescope = {
    enable = true;
    capSysNice = true;
    args = [
      "--adaptive-sync"
      "--steam"
      "--rt"
      "--expose-wayland"
      "--mangoapp"
    ];
  };

  hardware.xone.enable = true;
}
