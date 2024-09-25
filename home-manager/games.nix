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

    steam

    # FIXME build failure
    # (xivlauncher.override {
    #   steam =
    #     pkgs.steam.override {extraLibraries = pkgs: [pkgs.gamemode.lib];};
    # })
  ];
}
