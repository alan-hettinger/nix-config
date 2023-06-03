{ config, lib, pkgs, ... }: {
  home.packages = with pkgs;
    [
      (retroarch.override {
        cores = with libretro; [
          bsnes
          mupen64plus
          nestopia
          mgba
          steam
          xivlauncher

        ];
      })

    ];

}
