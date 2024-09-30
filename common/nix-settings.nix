{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  programs.nh = {
    enable = true;
    clean = {
      enable = true;
    };
    flake = "/home/alan/nix-config/";
  };
  nixpkgs.config = {
    allowUnfree = true;
  };
  nix = {
    channel.enable = false;
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
      experimental-features = "nix-command flakes no-url-literals";
      keep-derivations = false;
      keep-outputs = true;
      substituters = [
        "https://nix-community.cachix.org"
        "https://cachix.cachix.org"
        "https://hyprland.cachix.org"
        "https://cache.ngi0.nixos.org/"
      ];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
      max-jobs = "auto";
      min-free = 450000000000;
      show-trace = true;
      use-xdg-base-directories = true;
      trusted-users = [
        "root"
        "alan"
      ];
    };

    gc = {
      dates = "weekly";
      automatic = false;
      options = "--delete-older-than 30d";
    };
    package = pkgs.nixVersions.latest;
  };
}
