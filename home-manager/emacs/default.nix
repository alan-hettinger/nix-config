{pkgs, ...}: let
  myEmacsPkg = with pkgs; ((emacsPackagesFor emacs29-pgtk).emacsWithPackages (
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
          pkgs.ripgrep
        ];
        treemacsPkgs = [
          treemacs
          lsp-treemacs
          treemacs-evil
          treemacs-magit
          treemacs-persp
          treemacs-projectile
        ];
        vcPkgs = [
          magit
          # magit-tools
          # evil-magit
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
          solaire-mode
        ];
        orgPkgs = [
          evil-org
          org-superstar
          org-fancy-priorities
        ];
        langs = {
          nixLang = [
            nix-mode
            nix-update
          ];
          racket = [
            racket-mode
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
        ++ uiPkgs
        ++ orgPkgs
        ++ [
          vterm
          pdf-tools
          tree-sitter-langs
          general
          apheleia
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

  home.file.".emacs.d" = {
    source = ./config;
    recursive = true;
  };
}
