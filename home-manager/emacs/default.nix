{
  config,
  lib,
  pkgs,
  ...
}: let
  myEmacsPkg = with pkgs;
   ((emacsPackagesFor emacs29-pgtk).emacsWithPackages (
    epkgs: with epkgs; [
      evil
      evil-args
      evil-collection
      evil-easymotion
      evil-embrace
      evil-escape
      evil-indent-plus
      evil-markdown
      org-evil

      consult
      consult-flycheck
      embark
      embark-consult
      marginalia
      orderless
      vertico
      wgrep

      treemacs
      lsp-treemacs
      treemacs-evil
      treemacs-magit
      treemacs-persp
      treemacs-projectile

      magit
			# magit-tools
			# evil-magit

      vterm

      flycheck
      flycheck-popup-tip
      flycheck-posframe

      flyspell-correct
      flyspell-lazy

      pdf-tools

      doom-modeline
      nix-mode
      company-nixos-options
      nix-update

      evil-org
      org-superstar
      org-fancy-priorities

      racket-mode

      catppuccin-theme
    ]
  ));
in {
  home.packages = with pkgs; [
    git
    libvterm
    gcc
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

  xdg.configFile."emacs/init.el" = {
    source = ./init.el;
  };
}
