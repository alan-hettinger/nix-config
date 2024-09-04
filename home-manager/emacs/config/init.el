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

;; basic setup:
(setq sentence-end-double-space nil
      ring-bell-function 'ignore
      require-final-newline t
      confirm-kill-emacs 'y-or-n-p
      create-lockfiles nil
      uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "\\*"
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      x-stretch-cursor nil
      find-file-visit-truename t
      vc-follow-symlinks t
      find-file-suppress-same-file-warnings t
      use-short-answers t)
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              word-wrap t)
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

;; performance settings:
;; largely copied from doom-emacs.
(setq gc-cons-threshold (* 100 1024 1024)
      large-file-warning-threshold (* 100 1024 1024)
      redisplay-skip-fontification-on-input t)
(setq-default cursor-in-non-selected-windows nil)
(when (boundp 'pgtk-wait-for-event-timeout) ;; save performance on PGTK
  (setq pgtk-wait-for-event-timeout 0.001))

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

(defconst alan/mono-font "mononoki"
  "The mono-width font to use.")
(defconst alan/serif-font "Source Serif Variable")
(defconst alan/sans-font "Source Sans Variable")
(defconst alan/font-size 160)

(set-face-attribute 'default nil :family alan/mono-font :height alan/font-size)
(set-face-attribute 'variable-pitch nil :family alan/serif-font :height alan/font-size)
(require 'mixed-pitch)
(require 'ispell)
(setq mixed-pitch-set-height t
      ispell-dictionary "en_US")

(require 'catppuccin-theme)
(setq catppuccin-flavor 'macchiato
      catppuccin-italic-blockquotes nil
      catppuccin-highlight-matches t
      catppuccin-italic-variables nil)
(load-theme 'catppuccin t)

(add-hook 'global-hl-line-mode-hook
          (lambda () (let ((new-bg (catppuccin-get-color 'crust)))
                       (set-face-background 'hl-line new-bg))))

(setq enable-recursive-minibuffers t
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
