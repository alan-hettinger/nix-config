{pkgs, ...}: let
  emacsBasePkg = pkgs.emacs29-pgtk;
  treesitGrammars = (pkgs.emacsPackagesFor emacsBasePkg).treesit-grammars.with-all-grammars;

  myEmacsPkg = with pkgs; ((emacsPackagesFor emacsBasePkg).emacsWithPackages (
    epkgs:
      with epkgs; let
        evilModePkgs = [
          evil
          evil-args
          evil-collection
          evil-easymotion
          evil-embrace
          evil-escape
          evil-indent-plus
          evil-markdown
          org-evil
          evil-better-visual-line
        ];
        verticoSuitePkgs = [
          consult
          consult-flycheck
          embark
          embark-consult
          marginalia
          orderless
          vertico
          wgrep
          company-nixos-options
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
        ];
        uiPkgs = [
          doom-modeline
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
          evil-org
          org-superstar
          org-fancy-priorities
        ];
        diredPkgs = [
          dired-subtree
          dired-single
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
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science de es]))
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
  };
  # home.file.".emacs.d/tree-sitter" = {
  # source = "${treesitGrammars}/lib";
  # recursive = true;
  # };
}
