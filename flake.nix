{
  description = "Alan's Nix config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";

    stylix.url = "github:danth/stylix";
    hyprlock.url = "github:hyprwm/hyprlock";

    anyrun.url = "github:anyrun-org/anyrun";
    anyrun.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    nixpkgs,
    home-manager,
    nixos-hardware,
    nix-doom-emacs,
    stylix,
    hyprlock,
    anyrun,
    ...
  } @ inputs: let
    ## add my helper functions to lib
    mkLib = nixpkgs:
    # credit to https://github.com/kclejeune/system/blob/2ae7ced193f862ae3deace320c37f4657a873bd0/flake.nix#L49
      nixpkgs.lib.extend (final: prev: (import ./lib final) // home-manager.lib);
    lib = mkLib nixpkgs;

    systems = [
      "aarch64-linux"
      "i686-linux"
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ];

    forAllSystems = nixpkgs.lib.genAttrs systems;

    ## set the defaults for nixos system configs:
    generateNixosSystem = {
      ## typically just leave these first two as-is
      system ? "x86_64-linux",
      specialArgs ? {
        inherit inputs lib;
      },
      ## always provide a systemModule and at least one hardwareModules
      systemModule,
      hardwareModules,
      ## desktopModules are xorg/wayland modules, basic software, and whatever is needed for appearance
      desktopModules ? [
        ./appearance
        stylix.nixosModules.stylix
      ],
      ## plus one of xorg or wayland:
      enableXorg ? true,
      enableWayland ? false,
      ## hmModules includes whatever users are wanted
      hmModules ? [
        home-manager.nixosModules.home-manager
        {
          home-manager.extraSpecialArgs = {inherit inputs hyprlock;};
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "backup";
          # home-manager.users.alan = import ./home-manager/home.nix;
          home-manager.users.alan.imports = [
            ./home-manager/home.nix
            anyrun.homeManagerModules.anyrun
          ];
        }
      ],
      ## most customization occurs here:
      ## - modules for specific use-cases:
      games ? false,
      coding ? false,
      work ? true,
      media ? true,
      ## - system-specific stuff:
      extraModules ? [],
    }:
      nixpkgs.lib.nixosSystem {
        inherit system;
        inherit specialArgs;
        modules =
          systemModule
          ++ hardwareModules
          ++ desktopModules
          ++ extraModules
          ++ [./common]
          ++ [./pkgs]
          ++ (
            if enableXorg == true
            then [./desktop/xorg.nix]
            else []
          )
          ++ (
            if enableWayland == true
            then [./desktop/wayland.nix]
            else []
          )
          ++ hmModules
          ++ (
            if games == true
            then [./software/games.nix]
            else []
          )
          ++ (
            if coding == true
            then [./software/coding]
            else []
          )
          ++ (
            if work == true
            then [./software/work]
            else []
          )
          ++ (
            if media == true
            then [./software/media]
            else []
          );
      };
  in {
    packages = forAllSystems (system: import ./pkgs nixpkgs.legacyPackages.${system});

    nixosConfigurations = {
      alan-desktop-linux = generateNixosSystem {
        systemModule = [./systems/desktop];
        hardwareModules = [
          nixos-hardware.nixosModules.common-cpu-amd
          nixos-hardware.nixosModules.common-pc-ssd
          nixos-hardware.nixosModules.common-gpu-amd
        ];
        enableWayland = true;
        games = true;
        coding = true;
      };

      alan-laptop-linux = generateNixosSystem {
        systemModule = [./systems/laptop];
        hardwareModules = [nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen2];
        games = true;
      };
    };
  };
}
