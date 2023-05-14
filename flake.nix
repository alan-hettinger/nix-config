{
  description = "Alan's Nix config";

  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    # nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    ## just using all unstable for now
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager.url =
      "github:nix-community/home-manager/master"; # # remove the /master if switching to stable
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";

    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs =
    { nixpkgs, home-manager, nixos-hardware, nix-doom-emacs, ... }@inputs: {
      # NixOS configuration entrypoint
      # Available through 'sudo nixos-rebuild --flake .#your-hostname'
      nixosConfigurations = {
        alan-desktop = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; }; # Pass flake inputs to our config
          # > Our main nixos configuration file <
          modules = [ ./nixos/configuration.nix ];
        };
        alan-laptop = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; }; # Pass flake inputs to our config
          # > Our main nixos configuration file <
          modules = [
            ./nixos/configuration.nix
            nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen2
          ];
        };
      };

      # Standalone home-manager configuration entrypoint
      # Available through 'home-manager switch --flake .#your-username@your-hostname'
      #
      ## disabling this while I try using home-manager as a module
      #
      # homeConfigurations = {
      #   "alan@alan-desktop" = home-manager.lib.homeManagerConfiguration {
      #     pkgs =
      #       nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
      #     extraSpecialArgs = {
      #       inherit inputs;
      #     }; # Pass flake inputs to our config
      #     # > Our main home-manager configuration file <
      #     modules = [ ./home-manager/home.nix ];
      #   };

      # "alan@alan-laptop" = home-manager.lib.homeManagerConfiguration {
      #   pkgs =
      #     nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
      #   extraSpecialArgs = {
      #     inherit inputs;
      #   }; # Pass flake inputs to our config
      #   # > Our main home-manager configuration file <
      #   modules = [ ./home-manager/home.nix ];
      # };
      # };
    };
}
