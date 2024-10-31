{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    # https://nixos.wiki/wiki/RetroArch
    (retroarch.override {
      cores = with libretro; [
        bsnes
        # mupen64plus
        nestopia
        mgba
      ];
    })

    (xivlauncher.override {
      steam =
        pkgs.steam.override {extraLibraries = pkgs: [pkgs.gamemode.lib];};
    })
  ];

  programs.mangohud = {
    enable = true;
    enableSessionWide = true;
    settings = {
      gpu_temp = true;
      cpu_temp = true;
      fps = true;
    };
  };
}
