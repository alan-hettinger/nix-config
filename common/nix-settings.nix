{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  nixpkgs.config = {allowUnfree = true;};
  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath =
      lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
      config.nix.registry;

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      substituters = ["https://cachix.cachix.org" "https://hyprland.cachix.org"];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      ];
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
    };

    gc = {
      dates = "weekly";
      automatic = true;
      options = "--delete-older-than 30d";
    };
    package = pkgs.nixVersions.unstable;
  };
}
