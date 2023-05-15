{ pkgs, ... }: {
  imports = [
    ./common.nix
    ./desktop.nix
    ./docs.nix
    ./emacs.nix
    ./media.nix
    ./term.nix
    ./dev.nix
    # ./lang/default.nix #let's see if this works
    ./lang/go.nix
    ./lang/lisp.nix
    ./lang/misc.nix
    ./lang/rust.nix
    ./lang/cc.nix
    ./lang/haskell.nix
    ./lang/python.nix
    ./lang/js.nix
    ./themes/catppuccin.nix
    ./themes/fonts.nix
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
    };
  };

  # qt = { ## FIXME
  #   enable = true;
  #   platformTheme = "qt5ct";
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

    sessionPath = [ "$HOME/bin" "$HOME/.local/bin" "$HOME/.cargo/bin" ];

    sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qt5ct";
      TERM = "alacritty";
    };

    packages = with pkgs; [
      ripgrep
      fd
      fzf
      haskellPackages.greenclip
      # python311Packages.pip ## broke for some reason
      haskell-language-server # #installing here because haskellPackages.haskell-language-server didn't cooperate
    ];

    username = "alan";
    homeDirectory = "/home/alan";

    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    stateVersion = "22.11";
  };

  programs = {
    home-manager.enable = true;
    git.enable = true;
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
