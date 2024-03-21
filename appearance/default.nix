{ config, lib, pkgs, ... }: {
  # imports = lib.helperFunctions.getNixFilesFromDir ./.;
  imports = [ ./font.nix ./qt.nix ./stylix.nix ];
}
