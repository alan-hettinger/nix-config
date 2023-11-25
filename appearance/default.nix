{ config, lib, pkgs, ... }: {
  imports = lib.helperFunctions.getNixFilesFromDir ./.;
}
