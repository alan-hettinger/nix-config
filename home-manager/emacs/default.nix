{pkgs, ...}: let
  emacsBasePkg = pkgs.emacs29-pgtk;
  treesitGrammars = (pkgs.emacsPackagesFor emacsBasePkg).treesit-grammars.with-all-grammars;

  myEmacsPkg = with pkgs; ((emacsPackagesFor emacsBasePkg).emacsWithPackages (
    epkgs:
      with epkgs; let
        # TODO which of the following are actually used in use-package?
        evilModePkgs = [
          evil
          evil-args # TODO config?
          evil-collection
          evil-easymotion # TODO not used
          evil-embrace # TODO not used
          evil-escape # TODO not used
          evil-indent-plus # TODO not used
          evil-markdown # TODO not used
          evil-better-visual-line
        ];
        verticoSuitePkgs = [
          consult
          consult-flycheck
          consult-eglot
          embark
          embark-consult
          marginalia
          orderless
          vertico
          wgrep
          corfu
          corfu-candidate-overlay
          nerd-icons-corfu
          cape
        ];
        treemacsPkgs = [
          treemacs
          treemacs-evil
          treemacs-magit
          treemacs-projectile
          treemacs-nerd-icons
          treemacs-perspective
        ];
        vcPkgs = [
          magit
          # magit-tools
          hl-todo # not strictly vcs-related
          magit-todos
          diff-hl
        ];
        checkerPkgs = [
          flycheck
          flycheck-popup-tip
          flycheck-posframe
          flyspell-correct
          flyspell-lazy
          flycheck-eglot
        ];
        uiPkgs = [
          catppuccin-theme
          olivetti
          mixed-pitch
          which-key
          rainbow-delimiters
          highlight-quoted
          highlight-numbers
          solaire-mode
          visual-fill-column
          adaptive-wrap
          hide-mode-line
          anzu
          rainbow-mode
        ];
        orgPkgs = [
          org
          evil-org
          org-superstar
          org-fancy-priorities
          ox-pandoc
          citar
        ];
        diredPkgs = [
          dired-subtree
          dired-open
          all-the-icons-dired
        ];
        langs = {
          nixLang = [
            nix-mode
            nix-update
          ];
          racket = [
            racket-mode
          ];
          lisps = [
            lispy
            lispyville
            macrostep
          ];
          lua = [
            lua-mode
          ];
          web =
            # HTML/CSS
            [
              web-mode
            ];
        };
      in
        evilModePkgs
        ++ verticoSuitePkgs
        ++ treemacsPkgs
        ++ vcPkgs
        ++ checkerPkgs
        ++ langs.nixLang
        ++ langs.racket
        ++ langs.lisps
        ++ langs.lua
        ++ langs.web
        ++ uiPkgs
        ++ orgPkgs
        ++ diredPkgs
        ++ [
          vterm
          pdf-tools
          tree-sitter-langs
          treesit-auto
          # treesitGrammars
          general
          apheleia
          projectile-ripgrep
          projectile
          ibuffer-projectile
          all-the-icons-ibuffer
          helpful

          nov

          perspective
        ]
  ));
in {
  home.packages = with pkgs; [
    git
    libvterm
    gcc
    alejandra
    nil
    python3
    ripgrep
    lua-language-server
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science de es]))
    texlive.combined.scheme-full
  ];

  programs.emacs = {
    enable = true;
    package = myEmacsPkg;
  };

  services.emacs = {
    package = myEmacsPkg;
    enable = true;
    client.enable = true;
    defaultEditor = true;
    socketActivation.enable = true;
    startWithUserSession = "graphical";
  };

  home.file = {
    ".emacs.d/init.el" = {
      source = ./init.el;
    };
    ".emacs.d/early-init.el" = {
      source = ./early-init.el;
    };
    ".emacs.d/config" = {
      source = ./config;
      recursive = true;
    };
    ".emacs.d/config/config.el".source = ./config.el;
  };
  # home.file.".emacs.d/tree-sitter" = {
  # source = "${treesitGrammars}/lib";
  # recursive = true;
  # };
}
