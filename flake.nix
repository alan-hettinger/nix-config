{
  description = "Alan's Nix config";

  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    # nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    ## just using all unstable for now
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      # # remove the /master if switching to stable
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    hyprland.url = "github:hyprwm/Hyprland";
    stylix.url = "github:danth/stylix";
  };

  outputs = { nixpkgs, home-manager, nixos-hardware, nix-doom-emacs, hyprland
    , stylix, ... }@inputs:

    let
      mkLib = nixpkgs:
        # credit to https://github.com/kclejeune/system/blob/2ae7ced193f862ae3deace320c37f4657a873bd0/flake.nix#L49
        nixpkgs.lib.extend
        (final: prev: (import ./lib final) // home-manager.lib);

      lib = (mkLib nixpkgs);

    in {
      # Available through 'sudo nixos-rebuild --flake .#your-hostname'
      nixosConfigurations = {

        alan-desktop-linux = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs lib; };
          modules = [
            ./desktop/xorg.nix
            ./systems/desktop
            ./software/games.nix
            ./appearance
            ./common/common.nix
            # ./helper-functions.nix
            stylix.nixosModules.stylix
            nixos-hardware.nixosModules.common-cpu-amd
            nixos-hardware.nixosModules.common-pc-ssd
            nixos-hardware.nixosModules.common-gpu-amd

            home-manager.nixosModules.home-manager
            {
              # home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.alan = import ./home-manager/home.nix;
            }

            # hyprland.nixosModules.default
            # hyprland.homeManagerModules.default
          ];
        };

        alan-laptop-linux = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [
            ./desktop/xorg.nix
            ./systems/laptop
            ./appearance
            ./common/common.nix
            stylix.nixosModules.stylix
            nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen2

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.alan = import ./home-manager/home.nix;
            }

            # hyprland.nixosModules.default
            # hyprland.homeManagerModules.default
          ];
        };
      };
    };
}
