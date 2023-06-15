{ pkgs, ... }: {
  imports = [
    ./common.nix
    ./desktop.nix
    ./docs.nix
    ./emacs.nix
    ./media.nix
    ./term.nix
    ./dev.nix # # FIXME has an issue about openssl being marked insecure
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

  services.unclutter = {
    enable = true;
    threshold = 5;
    timeout = 30;
    extraOptions = [ "exclude-root" "ignore-scrolling" ];
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

    packages = with pkgs; [
      # python311Packages.pip ## broke for some reason
      haskell-language-server # #installing here because haskellPackages.haskell-language-server didn't cooperate
      transmission-gtk
    ];

    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    stateVersion = "22.11";
  };
}
