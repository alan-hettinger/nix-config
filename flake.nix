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
  };

  outputs =
    { nixpkgs, home-manager, nixos-hardware, nix-doom-emacs, hyprland, ... }@inputs: {
      # Available through 'sudo nixos-rebuild --flake .#your-hostname'
      nixosConfigurations = {

        alan-desktop-linux = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [
            ./nixos/configuration-desktop.nix
            nixos-hardware.nixosModules.common-cpu-amd
            nixos-hardware.nixosModules.common-pc-ssd
            nixos-hardware.nixosModules.common-gpu-amd
            hyprland.nixosModules.default
            hyprland.homeManagerModules.default
          ];
        };

        alan-laptop-linux = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [
            ./nixos/configuration-laptop.nix
            nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen2
          ];
        };
      };
    };
}
