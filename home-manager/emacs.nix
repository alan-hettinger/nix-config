{ inputs, pkgs, lib, config, ... }: {

  imports = [ inputs.nix-doom-emacs.hmModule ];

  home.packages = with pkgs; [
    emacsPackages.vterm
    libvterm
    emacsPackages.emacsql-sqlite
    lldb
    gdb
    unzip
    poppler
    gcc
    # (aspellWithDicts (dicts: with dicts; [ en en-computers en-science de es ]))
    hunspell
    hunspellDicts.de_DE
    hunspellDicts.en_US
    hunspellDicts.es_MX # they don't have es_US for some reason
    hunspellDicts.tok # toki pona just for fun
    ripgrep
    fd
    lua-language-server
    # clang
    texlive.combined.scheme-medium
    nil # nix lsp
    # (python38.withPackages (ps: with ps; [ jupyter ]))

    jdk # Java is required by EmmyLua LSP. Gross.

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

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./dotfiles/doom.d;
  };

  ## This way of linking allows .doom.d to be tracked by nix but disallows editing on the fly
  # home.file.".doom.d" = {
  #   source = ./dotfiles/doom.d;
  #   recursive = true;
  # };
  ## This way of doing things allows editing/reloading on the fly but means .doom.d is impure/stateful
  # home.activation = {
  #   installDoomDir = ''
  #     if [ ! -d "${config.home.homeDirectory}/.doom.d" ]; then
  #        ln -s "/home/alan/nix-config/home-manager/dotfiles/doom.d" "${config.home.homeDirectory}/.doom.d"
  #     fi
  #   '';
  # };
  ## disabling the above because nix-doom-emacs can deal with .doom.d

  # home.file.".config/awesome" = {
  #   source = ./dotfiles/awesome;
  #   recursive = true;
  # };

}
