{ inputs, lib, config, pkgs, ... }: {
  imports = [
    ./desktop.nix
    ./docs.nix
    ./emacs.nix
    ./media.nix
    ./term.nix
    ./dev.nix
    ./font.nix
    ./lang/go.nix
    ./lang/lisp.nix
    ./lang/misc.nix
    ./lang/rust.nix
    ./lang/cc.nix
    ./lang/haskell.nix
    ./lang/python.nix
    ./lang/js.nix
  ];

  nixpkgs = {
    # You can add overlays here
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
    };
  };

  # qt = {
  #   enable = true;
  #   platformTheme = "qt5ct";
  # };

  gtk = {
    enable = true;
    cursorTheme = {
      package = pkgs.catppuccin-cursors.macchiatoDark;
      name = "macchiatoDark";
      size = 48;
    };
    theme = {
      name = "Catppuccin-Macchiato-Compact-Rosewater-Dark";
      package = pkgs.catppuccin-gtk.override {
        size = "compact";
        variant = "macchiato";
        accents = [ "rosewater" ];
        tweaks = [ "rimless" ];
      };
    };
    font = {
      name = "Source Sans Pro";
      size = 16;
    };
  };

  home.pointerCursor = {
    package = pkgs.catppuccin-cursors.macchiatoDark;
    name = "macchiatoDark";
    gtk.enable = true;
    x11.enable = true;
    size = 48;
  };

  services.unclutter = {
    enable = true;
    threshold = 5;
    timeout = 30;
    extraOptions = [ "exclude-root" "ignore-scrolling" ];
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };

  services.gnome-keyring = { enable = true; };

  home.sessionPath =
    [ "$HOME/bin" "$HOME/.local/bin" "$HOME/.emacs.d/bin" "$HOME/.cargo/bin" ];

  home.sessionVariables = {
    QT_QPA_PLATFORMTHEME = "qt5ct";
    TERM = "alacritty";
    RANGER_LOAD_DEFAULT_RC = "false";
    XCURSOR_SIZE = "48";
  };

  home.packages = with pkgs; [
    # basic utilities
    ripgrep
    fd
    fzf
    unclutter-xfixes
    haskellPackages.greenclip
    gnome.gnome-font-viewer
    # python311Packages.pip ## broke for some reason

    haskell-language-server # #installing here because haskellPackages.haskell-language-server didn't cooperate

    ## extras:
    # nyxt
  ];

  # TODO: Set your username
  home = {
    username = "alan";
    homeDirectory = "/home/alan";
  };

  services.home-manager = {
    autoUpgrade = {
      enable = true;
      frequency = "weekly";
    };
  };

  # Enable home-manager and git
  programs.home-manager.enable = true;
  programs.git.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "22.11";
}
