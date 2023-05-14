{ pkgs, lib, config, ... }: {

  home.packages = with pkgs; [
    emacs
    emacsPackages.vterm
    libvterm
    emacsPackages.emacsql-sqlite
    lldb
    gdb
    unzip
    poppler
    gcc
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science de es ]))
    ripgrep
    fd
    # lua-language-server
    # clang
    texlive.combined.scheme-medium
    # (python38.withPackages (ps: with ps; [ jupyter ]))

    nixfmt
    languagetool
  ];

  services.emacs = {
    enable = true;
    client.enable = true;
    defaultEditor = true;
    socketActivation.enable = true;
    startWithUserSession = true;
    # extraOptions = [  ];
  };

  ## This way of linking allows .doom.d to be tracked by nix but disallows editing on the fly
  # home.file.".doom.d" = {
  #   source = ./dotfiles/doom.d;
  #   recursive = true;
  # };
  ## This way of doing things allows editing/reloading on the fly but means .doom.d is impure/stateful
  home.activation = {
    installDoomDir = ''
      if [ ! -d "${config.home.homeDirectory}/.doom.d" ]; then
         ln -s "/home/alan/nix-config/home-manager/dotfiles/doom.d" "${config.home.homeDirectory}/.doom.d"
      fi
    '';
  };

  # home.file.".config/awesome" = {
  #   source = ./dotfiles/awesome;
  #   recursive = true;
  # };

}
