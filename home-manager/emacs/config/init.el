
;; FIXME:
(defun alan/load-hack (file-name)
  "Load .el files from .emacs.d or equivalent.
   Needed because emacs can't find those files, likely because of symlinks related to nixos."
  (load-file (format "%s%s.el" user-emacs-directory file-name)))

(add-hook 'after-init-hook (lambda () (alan/load-hack "ui-config")))
(add-hook 'after-init-hook (lambda () (alan/load-hack "completion-config")))
(add-hook 'after-init-hook (lambda () (alan/load-hack "programming-config")))
(add-hook 'dired-mode-hook (lambda () (alan/load-hack "dired-config")))

;; start tree-sitter:
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(set-face-attribute 'default nil :font "mononoki" :height 160)
(require 'mixed-pitch)
(setq mixed-pitch-set-height t
      ispell-dictionary "en_US")
;; (set-face-attribute 'variable-pitch nil :height 1.2)

(setq catppuccin-flavor 'macchiato
      catppuccin-italic-blockquotes nil
      catppuccin-highlight-matches t
      catppuccin-italic-variables nil)
(load-theme 'catppuccin t)

(add-hook 'global-hl-line-mode-hook
          (lambda () (let ((new-bg (catppuccin-get-color 'crust)))
                       (set-face-background 'hl-line new-bg))))

(setq enable-recursive-minibuffers t
      split-height-threshold nil
      split-width-threshold 40
      default-frame-alist '((undecorated . t))
      frame-title-format '("%b"))

(add-hook 'org-mode-hook (lambda () (alan/load-hack "org-config")))

(setq evil-want-keybinding nil
      evil-want-fine-undo t
      evil-undo-system 'undo-redo
      evil-want-integration t)
(evil-collection-init)
(add-hook 'evil-mode-hook #'evil-better-visual-line-on)
(evil-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(require 'apheleia)
(apheleia-global-mode +1)
(push '(nixfmt . ("alejandra")) apheleia-formatters)

(setq olivetti-style 'fancy
      olivetti-body-width 90)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))
(add-hook 'nix-mode-hook #'eglot-ensure)

(defun alan/treemacs-setup ()
  (progn (setq treemacs-indentation 1
               treemacs-indentation-string "┃"
               treemacs-width 35
               treemacs-wide-toggle-width 40
               treemacs-text-scale 0.5
	       treemacs-icon-size 10
               treemacs-is-never-other-window nil)
         (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
         (treemacs-git-commit-diff-mode t)
         (treemacs-git-mode 'extended)
         ;; (treemacs-indent-guide-mode t) ;; FIXME throws type error about arrayp
	 (treemacs-follow-mode t)
	 (with-eval-after-load 'evil 'treemacs-evil) ;; FIXME throws error about void-function. Is package on path?
	 ;; (with-eval-after-load 'magit (treemacs-magit)) ;; FIXME throws same void-function error
	 (display-line-numbers-mode -1)))
(add-hook 'treemacs-mode-hook #'alan/treemacs-setup)

(setq-default indent-tabs-mode nil)

(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)

(add-hook 'after-init-hook (alan/load-hack "keybinds"))

;; magit config:
;; TODO split into separate file
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(add-hook 'magit-mode-hook 'magit-todos-mode)

(add-hook 'after-init-hook #'projectile-mode)
(add-hook 'projectile-mode-hook
          (lambda ()
            (setq projectile-project-search-path '("~/nix-config/")
                  projectile-switch-project-action 'projectile-dired)))
