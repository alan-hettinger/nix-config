{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    emacs-gtk
    git
    emacsPackages.vterm
    libvterm
    emacsPackages.emacsql-sqlite
    lldb
    gdb
    unzip
    poppler
    gcc
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science de es]))

    ## hunspell dictionaries
    # hunspell
    # hunspellDicts.de_DE
    # hunspellDicts.en_US
    # hunspellDicts.es_MX
    # hunspellDicts.tok

    ripgrep
    fd
    lua
    lua-language-server
    # clang
    texlive.combined.scheme-full # FIXME should only need scheme-medium and this many latex packages is a pain
    nil # nix lsp
    # (python38.withPackages (ps: with ps; [ jupyter ]))

    jdk # Java is required by EmmyLua LSP. Gross.

    alejandra
    languagetool
  ];

  services.emacs = {
    package = pkgs.emacs-gtk;
    enable = true;
    client.enable = true;
    defaultEditor = true;
    socketActivation.enable = true;
    startWithUserSession = "graphical";
    # extraOptions = [  ];
  };

  # # nix-doom-emacs is having some issues at the moment, back to the old way
  # programs.doom-emacs = {
  #   enable = true;
  #   doomPrivateDir = ./dotfiles/doom.d;
  # };

  home.file.".doom.d" = {
    source = ./dotfiles/doom.d;
    recursive = true;
  };

  home.activation = {
    ## doom emacs can't be installed using fetchFromGithub because some doom scripts modify the contents of the directory
    installDoom = ''
      if [ ! -d "${config.home.homeDirectory}/.emacs.d" ]; then
      ${pkgs.git}/bin/git clone --depth 1 https://github.com/doomemacs/doomemacs ${config.home.homeDirectory}/.emacs.d
      fi
    '';
  };
}
