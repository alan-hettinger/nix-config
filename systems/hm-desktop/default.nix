{lib, ...}: {
  imports = lib.helperFunctions.getNixFilesFromDir ./.;
}
