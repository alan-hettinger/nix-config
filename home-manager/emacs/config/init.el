;;; package --- init.el  -*- lexical-binding: t -*-

;;; Commentary:
;;; Entry-point for my Emacs config. Packages are installed via nix.

;;; Code:

;; FIXME: this hack shouldn't be necessary
(defun alan/load-hack (file-name)
  "Load FILE-NAME.el files from .emacs.d or equivalent.
Needed because Emacs can't find those files,
likely because of symlinks related to nixos."
  (load-file (format "%s%s.el" user-emacs-directory file-name)))

(alan/load-hack "user-consts")

;; basic setup:
(setq sentence-end-double-space nil
      ring-bell-function 'ignore
      require-final-newline t
      confirm-kill-emacs 'y-or-n-p
      create-lockfiles nil
      ;; ^ TODO do I need this after all because I use emacs server?
      uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "\\*"
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      x-stretch-cursor nil
      find-file-visit-truename t
      vc-follow-symlinks t
      find-file-suppress-same-file-warnings t
	  backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/"
                                                         user-emacs-directory)))
      use-short-answers t)
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              word-wrap t)
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-prune-cache t))
(global-auto-revert-mode 1)
(require 'autorevert)
(setq auto-revert-verbose nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(fset 'yes-or-no-p 'y-or-n-p)
(set-language-environment "UTF-8")
;; set-language-environment overrides default-input-method:
(setq default-input-method nil)
;; Scrolling (largely copied from Doom)
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 2
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; set up the custom file in case the customization interface is desired.
;; note that this file is not tracked by git.
(defvar alan/custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file alan/custom-file)
(unless (file-exists-p alan/custom-file)
  (make-empty-file alan/custom-file))
(load-file alan/custom-file)

;; TODO make following lines DRYer
(add-hook 'after-init-hook (lambda () (alan/load-hack "ui-config")))
(add-hook 'after-init-hook (lambda () (alan/load-hack "completion-config")))
(add-hook 'after-init-hook (lambda () (alan/load-hack "programming-config")))
(add-hook 'dired-mode-hook (lambda () (alan/load-hack "dired-config")))

;; start tree-sitter:
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(set-face-attribute 'default nil :family alan/mono-font :height alan/font-size)
(set-face-attribute 'variable-pitch nil :family alan/serif-font :height alan/font-size)
(require 'mixed-pitch)
(require 'ispell)
(setq mixed-pitch-set-height t
      ispell-dictionary "en_US"
      ispell-complete-word-dict "en_US")

(defun alan/enable-catppuccin-theme ()
  (progn (require 'catppuccin-theme)
         (setq catppuccin-flavor 'macchiato
               catppuccin-italic-blockquotes nil
               catpuccin-highlight-matches t
               catppuccin-italic-variables nil)
         (load-theme 'catppuccin t)
         (add-hook 'global-hl-line-mode-hook
                   (lambda () (let ((new-bg (catppuccin-get-color 'crust)))
                                (set-face-background 'hl-line new-bg))))
         ;; HACK refresh global-hl-line-mode if it is already enabled to reapply hook
         (when global-hl-line-mode (global-hl-line-mode 1))
         (setq alan/theme-initialized-p t)))
(fset 'alan/enable-theme 'alan/enable-catppuccin-theme)
(unless alan/theme-initialized-p (alan/enable-theme))

(setq enable-recursive-minibuffers t
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

(require 'treemacs)
(defun alan/before-treemacs-setup ()
  (setq treemacs-indentation 1
        treemacs-indentation-string "┃"
        treemacs-width 35
        treemacs-wide-toggle-width 40
        treemacs-text-scale -1))
(defun alan/treemacs-setup ()
  (progn
    (treemacs-git-mode -1)
    ;; ^ FIXME enabling treemacs git mode causes emacs to hang.
    ;; Traced issue to "treemacs-process-file-events".
	(treemacs-follow-mode nil)
	;; ^ FIXME enabling follow mode causes error:
    ;; "error running timer 'treemacs--follow' wrong type argument arrayp nil"
    (treemacs-follow-mode 1)
    (display-line-numbers-mode -1)))
(add-hook 'after-init-hook #'alan/before-treemacs-setup)
(add-hook 'treemacs-mode-hook #'alan/treemacs-setup)

(setq-default indent-tabs-mode nil)

(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)

(add-hook 'after-init-hook (alan/load-hack "keybinds"))

;; magit config:
;; TODO split into separate file
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
      magit-diff-refine-hunk t)
(add-hook 'magit-mode-hook 'magit-todos-mode)

(add-hook 'after-init-hook #'projectile-mode)
(add-hook 'projectile-mode-hook
          (lambda ()
            (setq projectile-project-search-path '("~/nix-config/")
                  projectile-switch-project-action 'projectile-dired)))
(add-hook 'diff-hl-mode-hook #'diff-hl-margin-mode)
(add-hook 'diff-hl-mode-hook
          (lambda () (setq diff-hl-global-modes '(not image-mode pdf-view-mode)
                           diff-hl-update-async t)))
(add-hook 'after-init-hook #'global-diff-hl-mode)

;; recentf mode:
(add-hook 'recentf-mode-hook
          (lambda ()
            (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
                  recentf-max-saved-items 1000
                  recentf-max-menu-items 30
                  recentf-auto-cleanup 'never)))
(add-hook 'after-init-hook (lambda () (recentf-mode +1)))

(defun alan/ibuffer-setup ()
  (progn (ibuffer-projectile-set-filter-groups)
         (all-the-icons-ibuffer-mode)
         (setq ibuffer-expert t
               ibuffer-show-empty-filter-groups nil
               all-the-icons-ibuffer-icon-size 0.8
               ibuffer-default-sorting-mode 'major-mode)
         (define-ibuffer-column size
           ;; Make size column human-readable
           (:name "Size"
                  :inline t
                  :header-mouse-map ibuffer-size-header-map)
           (file-size-human-readable (buffer-size)))))
(add-hook 'ibuffer-hook #'alan/ibuffer-setup)

;; TODO "server-after-make-frame-hook"

(provide 'init)
;;; init.el ends here
