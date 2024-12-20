;;; package --- init.el  -*- lexical-binding: t -*-

;;; Commentary:
;;; Entry-point for my Emacs config. Packages are installed via nix.

;;; Code:

;;; Add own config files to the load path so we can require them:
;;; (Shouldn't be necessary but likely something with nixos.)
(add-to-list 'load-path (expand-file-name "./config/" user-emacs-directory))

;; Load user-consts early so other files can reference them.
(require 'user-consts)

(use-package package
  :custom
  ;; Always use the system-installed version of a package (through NixOS),
  ;; never install through use-package or package-install
  (package-archives nil))
(use-package use-package
  ;; Settings for use-package itself:
  :defer nil
  :demand t
  :init
  (setq use-package-always-ensure nil
        ;; other package defaults:
        use-package-compute-statistics t
        use-package-always-defer t))

;; Need general before init so that ':general' tags in use-package evaluate
(add-hook 'before-init-hook (require 'keybinds))


;;; TODO: could be useful to wrap my local settings in a group for documentation.
(defgroup alan-settings nil
  "Customization group for my local settings."
  :prefix "alan/"
  :group 'local)


(use-package emacs
  ;; Pseudo-package to set up built-in functionality
  :demand t
  :defer nil
  :init
  (when (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors 'silent
          native-comp-prune-cache t))
  :custom (sentence-end-double-space nil)
  (ring-bell-function 'ignore)
  (require-final-newline t)
  (confirm-kill-emacs 'y-or-n-p)
  (create-lockfiles nil) ;; TODO do I need this after all for server?
  (uniquify-buffer-name-style 'forward)
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "\\*")
  (highlight-nonselected-windows nil)
  (fast-but-imprecise-scrolling t)
  (x-stretch-cursor nil)
  (find-file-visit-truename t)
  (vc-follow-symlinks t)
  (find-file-suppress-same-file-warnings t)
  (backup-directory-alist
   `(("." . ,(expand-file-name "tmp/backups/" alan/cache-dir))))
  (auto-save-list-file-prefix
   (expand-file-name "auto-save-list/.saves-" alan/cache-dir))
  (use-short-answers t)
  (default-input-method nil)
  ;; Scrolling:
  (hscroll-margin 2)
  (hscroll-step 1)
  (scroll-conservatively 10)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  (mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 2)
  (mouse-wheel-progressive-speed nil)
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; delete all whitespace on backspace, including newlines
  ;; could also be `hungry' to exclude newlines
  ;; TODO this should work differently for languages with significant whitespace
  (backward-delete-char-untabify-method 'all)

  :config
  (setq-default indent-tabs-mode nil
                tab-width 4
                fill-column 80
                word-wrap t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-language-environment "UTF-8")
  (setq user-full-name "Alan Hettinger"
        user-mail-address "alan.hettinger@proton.me"))

(use-package autorevert
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode 1))

(use-package bookmark
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" alan/cache-dir)))

;; set up the custom file in case the customization interface is desired.
;; note that this file is not tracked by git.
(defvar alan/custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file alan/custom-file)
(unless (file-exists-p alan/custom-file)
  (make-empty-file alan/custom-file))
(load-file alan/custom-file)

;; TODO 2024-09 - better control flow in these lines.
;; Errors if change eval order.
(require 'completion-config)
(require 'ui-config)
(require 'alternate-modeline)
(require 'eshell-config)
(require 'dired-config)
(require 'vc-config)
(add-hook 'after-init-hook
          (lambda ()
            (progn
              (require 'programming-config)
              (require 'evil-config)
              (require 'org-config))))

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package ispell
  :hook ((prog-mode text-mode) . ispell-minor-mode)
  :custom (ispell-dictionary "en_US")
  (ispell-complete-word-dict "en_US"))

(use-package hl-line
  :hook ((prog-mode text-mode ibuffer-mode dired-mode) . hl-line-mode))

(setq enable-recursive-minibuffers t
      frame-title-format '("%b"))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t))

(setq-default indent-tabs-mode nil)

(use-package highlight-quoted
  :hook emacs-lisp-mode)

(use-package projectile
  :hook (after-init . projectile-mode)
  :config
  (add-hook 'projectile-before-switch-project-hook
            (lambda () (when persp-mode (persp-switch "project-switching"))))
  (add-hook 'projectile-after-switch-project-hook
            (lambda () (when persp-mode (persp-rename (projectile-project-name)))))
  :general
  (alan/leader-keys
    "p" '(projectile-command-map :which-key "project"))
  :custom
  (projectile-project-search-path '("~/nix-config/"))
  (projectile-switch-project-action 'projectile-dired))

(use-package visual-fill-column
  :hook ((text-mode prog-mode dired-mode) . visual-line-fill-column-mode)
  :init
  (defun alan/visual-fill-prog ()
    (setq-local visual-fill-column-width 80)
    (when (and (boundp 'diff-hl-mode) diff-hl-mode) (setq-local diff-hl-side 'right)))
  (add-hook 'prog-mode-hook #'alan/visual-fill-prog)
  (defun alan/visual-fill-text-setup ()
    (setq-local visual-fill-column-width 120))
  (add-hook 'org-mode-hook #'alan/visual-fill-text-setup)
  :config
  (setq-default visual-fill-column-width 80)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-enable-sensible-window-split t)
  (visual-fill-column-fringes-outside-margins t))

(use-package adaptive-wrap
  :hook (visual-line-fill-column-mode . adaptive-wrap-prefix-mode))

(use-package diff-hl
  :hook prog-mode
  :after visual-fill-column
  :init (setq diff-hl-global-modes '(not image-mode pdf-view-mode)
              diff-hl-update-async t
              diff-hl-side (if visual-fill-column-mode
                               'right
                             'left))
  :config
  (diff-hl-margin-mode))

(use-package helpful
  :init
  (alan-ui/display-buffer-enforce-side-popup 'helpful-mode)
  :general
  ([remap describe-function] #'helpful-callable
   [remap describe-key] #'helpful-key
   [remap describe-variable] #'helpful-variable
   [remap view-hello-file] #'helpful-at-point))

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


(use-package pdf-tools)

(use-package vterm
  :commands (alan-vterm/toggle vterm-mode)
  :init
  (alan-ui/display-buffer-enforce-side-popup 'vterm-mode)
  (defun alan-vterm/toggle (arg)
    "Toggles a terminal popup window at project root.
If prefix ARG is non-nil, recreate vterm buffer in the current project's root.
Returns the vterm buffer.
Shamelessly stolen from Doom."
    (interactive "P")
    (let ((buffer-name
           (format "*vterm-popup:%s*"
                   (if (bound-and-true-p persp-mode)
                       (persp-current-name)
                     "main")))
          confirm-kill-processes
          current-prefix-arg)
      (when arg
        (let ((buffer (get-buffer buffer-name))
              (window (get-buffer-window buffer-name)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (when (window-live-p window)
            (delete-window window))))
      (if-let (win (get-buffer-window buffer-name))
          (delete-window win)
        (let ((buffer (or
                       (get-buffer buffer-name)
                       (get-buffer-create buffer-name))))
          (with-current-buffer buffer
            (setq-local +vterm--id buffer-name)
            (unless (eq major-mode 'vterm-mode)
              (vterm-mode)))
          (pop-to-buffer buffer)))
      (get-buffer buffer-name)))

  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-copy-exclude-prompt t)
  :general
  (alan/leader-keys
    "ot" '(alan-vterm/toggle :which-key "vterm"))
  )

;; TODO "server-after-make-frame-hook"

(provide 'init)
;;; init.el ends here
