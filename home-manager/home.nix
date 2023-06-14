{ pkgs, ... }: {
  imports = [
    ./common.nix
    ./desktop.nix
    ./docs.nix
    ./emacs.nix
    ./media.nix
    ./term.nix
    ./dev.nix ## FIXME has an issue about openssl being marked insecure
    ./lang/go.nix
    ./lang/lisp.nix
    ./lang/misc.nix
    ./lang/rust.nix
    ./lang/cc.nix
    ./lang/haskell.nix
    ./lang/python.nix
    ./lang/js.nix
    ./themes/catppuccin/catppuccin.nix
    ./themes/fonts.nix
    ./games.nix
  ];

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
      permittedInsecurePackages = [ ## TODO this is a temporary fix due to above error, test after update if still present
        "openssl-1.1.1u" ## TODO find who needs this
        # "nodejs-16.20.0" ## I think the problem package here is bitwarden?
      ];
    };
  };

  # qt = {
  #   enable = true;
  #   platformTheme = "gtk";
  # };

  gtk.enable = true;

  services = {
    unclutter = {
      enable = true;
      threshold = 5;
      timeout = 30;
      extraOptions = [ "exclude-root" "ignore-scrolling" ];
    };

    gnome-keyring = { enable = true; };

    home-manager = {
      autoUpgrade = {
        enable = true;
        frequency = "weekly";
      };
    };
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };
  xdg.dataFile = {
    scripts = {
      enable = true;
      executable = true;
      recursive = true;
      source = ./scripts;
    };
  };

  ## setting default applications when opening in dolphin, firefox, etc:
  # xdg.mimeApps = {
  #   enable = true;
  #   associations = {
  #     defaultApplications = {
  #       "application/pdf" = [ "okular.desktop" ];
  #       "text" = [ "emacsclient.desktop" ]; ## just all text files all the time
  #     };
  #   };
  # };
  
  home = {

    pointerCursor = {
      # package = pkgs.catppuccin-cursors.macchiatoDark;
      # name = "macchiatoDark";
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      gtk.enable = true;
      x11.enable = true;
      size = 48;
    };

    sessionPath = [ "$HOME/bin" "$HOME/.local/share/scripts" "$HOME/.cargo/bin" ];

    sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qt5ct";
      TERM = "alacritty";
      GOPATH = "$HOME/go";
    };

    packages = with pkgs; [
      ripgrep
      fd
      fzf
      # python311Packages.pip ## broke for some reason
      haskell-language-server # #installing here because haskellPackages.haskell-language-server didn't cooperate
      transmission-gtk
    ];

    username = "alan";
    homeDirectory = "/home/alan";

    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    stateVersion = "22.11";
  };

  programs = {
    home-manager.enable = true;
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
