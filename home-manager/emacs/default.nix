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
          hl-todo # not strictly vcs-related
          magit-todos
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
        ++ diredPkgs
        ++ [
          vterm
          pdf-tools
          tree-sitter-langs
          (treesit-grammars.with-grammars (grammars:
            with grammars; [
              tree-sitter-bash
              tree-sitter-c
              tree-sitter-cpp
              tree-sitter-css
              tree-sitter-html
              tree-sitter-javascript
              tree-sitter-typescript
              tree-sitter-json
              tree-sitter-yaml
              tree-sitter-clojure
              tree-sitter-commonlisp
              tree-sitter-elisp
              tree-sitter-go
              tree-sitter-latex
              tree-sitter-lua
              tree-sitter-nix
              tree-sitter-python
              tree-sitter-regex
              tree-sitter-rust
              tree-sitter-scheme
            ]))
          general
          apheleia
          projectile-ripgrep
          projectile
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
