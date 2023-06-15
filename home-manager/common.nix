{ config, lib, pkgs, ... }: {

  nixpkgs = {
    overlays = [
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages =
        [ # # TODO this is a temporary fix due to above error, test after update if still present
          "openssl-1.1.1u" # # TODO find who needs this
          # "nodejs-16.20.0" ## I think the problem package here is bitwarden?
        ];
    };
  };

  services = {
    gnome-keyring = { enable = true; };
    home-manager = {
      autoUpgrade = {
        enable = true;
        frequency = "weekly";
      };
    };
  };

  gtk.enable = true;

  xdg = {
    userDirs = {
      enable = true;
      createDirectories = true;
    };
    dataFile = {
      scripts = {
        enable = true;
        executable = true;
        recursive = true;
        source = ./scripts;
      };
    };
  };

  programs = {

    git = {
      enable = true;
      delta = { enable = true; };
      userName = "Alan";
      userEmail = "alan.hettinger@proton.me";
    };
    gh = {
      enable = true;
      enableGitCredentialHelper = true;
      settings = { };
    };

    firefox = {
      enable = true;
      profiles = {
        alan = {
          isDefault = true;
          userChrome = builtins.readFile ./dotfiles/firefox/userChrome.css;
          extraConfig = builtins.readFile ./dotfiles/firefox/user.js;
        };
      };
    };

    home-manager.enable = true;
  };
  home = {
    sessionPath =
      [ "$HOME/bin" "$HOME/.local/share/scripts" "$HOME/.cargo/bin" ];

    sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qt5ct";
      TERM = "alacritty";
      GOPATH = "$HOME/go";
    };

    username = "alan";
    homeDirectory = "/home/alan";

    packages = with pkgs; [
      ripgrep
      fd
      fzf

    ];
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
