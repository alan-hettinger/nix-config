{ lib, ... }:

## FIXME what I want this code to do is include all the contents of PWD except for itself as imports so that I can import the whole directory elsewhere. Currently getting build error about possible infinite recursion
## 
## credit to https://github.com/evanjs/nixos_cfg/blob/4bb5b0b84a221b25cf50853c12b9f66f0cad3ea4/config/new-modules/default.nix

with lib;
let
  # Recursively constructs an attrset of a given folder, recursing on directories, value of attrs is the filetype
  getDir = dir:
    mapAttrs
    (file: type: if type == "directory" then getDir "${dir}/${file}" else type)
    (builtins.readDir dir);

  # Collects all files of a directory as a list of strings of paths
  files = dir:
    collect isString
    (mapAttrsRecursive (path: type: concatStringsSep "/" path) (getDir dir));

  # Filters out directories that don't end with .nix or are this file, also makes the strings absolute
  validFiles = dir:
    map (file: ./. + "/${file}") (filter (file:
      hasSuffix ".nix" file && file != "default.nix"
      && !lib.hasPrefix "x/taffybar/" file && !lib.hasSuffix "-hm.nix" file)
      (files dir));

in {

  imports = validFiles ./.;

}

