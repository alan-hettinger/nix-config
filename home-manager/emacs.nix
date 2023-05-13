{ pkgs, ... }: {

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
    lua-language-server
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

}
