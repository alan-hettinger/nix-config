lib: basePath: {

  getNixFilesFromDir = dir:
    # path -> list
    # Items are the absolute path of each .nix file in the input path, recursively, without default.nix
    # inspired by: https://github.com/evanjs/nixos_cfg/blob/4bb5b0b84a221b25cf50853c12b9f66f0cad3ea4/config/new-modules/default.nix
    let
      # helper functions:
      getDir = dir:
        # path -> set of { "filename" = "type" ("regular" or "{ ... }") }
        lib.mapAttrs (file: type:
          if type == "directory" then getDir "${dir}/${file}" else type)
        (builtins.readDir dir);
      files = dir:
        # path -> list of strings
        # Strings are the relative path of each file in the input path, recursive across directories
        lib.collect lib.isString
        (lib.mapAttrsRecursive (path: type: lib.concatStringsSep "/" path)
          (getDir dir));

    in map (file: dir + "/${file}")
    (lib.filter (file: lib.hasSuffix ".nix" file && file != "default.nix")
      (files dir));
}
