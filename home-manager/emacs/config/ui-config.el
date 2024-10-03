;;; ui-config.el  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; some basic settings:
(setq-default truncate-partial-width-windows nil)
(setq
 echo-keystrokes 0.02
 auto-save-no-message t
 help-window-select t
 eval-expression-print-length nil
 kill-do-not-save-duplicates t
 scroll-error-top-bottom t)


(set-face-attribute 'default nil :family alan/mono-font :height alan/font-size)
(set-face-attribute 'variable-pitch nil :family alan/serif-font :height alan/font-size)


(defun alan-ui/display-buffer-enforce-side-popup (mode &optional side-select)
  "Add MODE to `display-buffer-alist' with popup window properties.
If SIDE-SELECT is provided, the popup will display on that side."
  (add-to-list 'display-buffer-alist
               `((derived-mode . ,mode)
                 (display-buffer-reuse-mode-window
                  display-buffer-in-side-window)
                 (side . ,(if side-select side-select 'right))
                 ;; `window-width' : int -> columns
                 ;;                : float -> portion of frame size
                 (window-width . 90)
                 (dedicated . t))))

(define-minor-mode alan/padding-mode
  "docstring"
  :init-value nil
  :interactive t
  :global t
  :group 'alan-ui
  (let ((b-width (if alan/padding-mode 15 0))
        (f-width (if alan/padding-mode 20 20))
        (padding-color (if alan/padding-mode
                           (face-attribute 'default :background)
                         nil)))
    (progn (modify-all-frames-parameters
            `((internal-border-width . ,b-width)
              (right-divider-width . ,b-width)
              (bottom-divider-width . ,(if alan/padding-mode 10 0))
              (left-fringe . ,f-width)
              (right-fringe . ,f-width)))
           (set-face-background 'fringe padding-color)
           (set-face-foreground 'window-divider padding-color)
           (dolist (face (list 'fringe
                               'internal-border))
             (set-face-background face padding-color))
           (dolist (face (list 'window-divider
                               'window-divider-first-pixel
                               'window-divider-last-pixel))
             (set-face-foreground face padding-color)))))


(use-package display-line-numbers
  :custom
  (display-line-numbers-width 3)
  (display-line-numbers-widen 3)
  (display-line-numbers-type t))


(use-package catppuccin-theme
  :defer nil
  :custom
  (catppuccin-flavor 'macchiato)
  (catppuccin-italic-blockquotes nil)
  (catppuccin-highlight-matches t)
  (catppuccin-italic-variables nil)
  :config
  (load-theme 'catppuccin t)
  (add-hook 'hl-line-mode-hook
            (lambda () (let ((new-bg (catppuccin-get-color 'crust)))
                         (set-face-background 'hl-line new-bg))))
  ;; TODO this function might not be needed now
  (defun alan/enable-catppuccin-theme ()
    (progn
      (load-theme 'catppuccin t)
      ;; HACK refresh global-hl-line-mode if it is already enabled to reapply hook
      (when global-hl-line-mode (global-hl-line-mode 1))
      (setq alan/theme-initialized-p t)))
  (fset 'alan/enable-theme 'alan/enable-catppuccin-theme))

;; Some standard modes:
(set-fringe-mode 20)
(setq flycheck-emacs-lisp-load-path 'inherit
      flycheck-disabled-checkers '(emacs-lisp-checkdock)
      flycheck-idle-change-delay 1.0
      flycheck-posframe-border-width 1
      flycheck-posframe-border-use-error-face t
      flycheck-indication-mode nil)
(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(global-flycheck-mode 1)
(add-hook 'hl-line-mode-hook
          (lambda () (setq hl-line-sticky-flag nil)))
(global-anzu-mode 1)

(defun alan/disable-scroll-bars (frame)
  "Hide scroll bars on creating any FRAME. Useful with Emacs server."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(defun alan/hide-minibuffer-scroll-bar (frame)
  "Hide the minibuffer scroll bar in FRAME."
  (set-window-scroll-bars (minibuffer-window frame) 0 nil 0 nil t))

(add-hook 'after-make-frame-functions
          (lambda (frame) (if alan/disable-all-scrollbars
                              (alan/disable-scroll-bars frame)
                            (alan/hide-minibuffer-scroll-bar frame))))

(setq alan-mode-line/left
      '("%e"
        (:eval alan-mode-line/evil-mode-line-tag)
        (:eval (alan-mode-line/ace-window-display))
        (:eval (alan-mode-line/show-only-on-selected mode-line-position))
        (:eval alan-mode-line/word-count)
        (:eval alan-mode-line/defining-kbd-macro)))
(setq alan-mode-line/center
      '((:eval alan/modeline-mode-icon)
        (:eval alan-mode-line/buffer-title)))
(setq alan-mode-line/right
      '(
        (:eval (anzu--update-mode-line))
        (:eval (alan-mode-line/show-only-on-selected mode-line-misc-info))
	    (:eval (alan-mode-line/vc-mode-display))
        "  "
        (:eval alan-mode-line/major-mode-name)))

(ace-window-display-mode 1)

(require 'solaire-mode)
(solaire-global-mode +1)

;; highlight number literals:
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'highlight-numbers-mode-hook
          (lambda ()
            (setq highlight-numbers-generic-regexp
                  "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>")))

;; Perspective mode:
(use-package perspective
  :init
  (defconst alan/perspective-state-file
    (expand-file-name "persp-save" alan/cache-dir))
  (unless (file-exists-p alan/perspective-state-file)
    (make-empty-file alan/perspective-state-file))
  (persp-mode)
  :general
  (alan/leader-keys "TAB" '(perspective-map :which-key "perspective"))
  :custom
  (persp-suppress-no-prefix-key-warning t) ;; not needed with general map
  (persp-initial-frame-name "main")
  (persp-sort 'created)
  (persp-state-default-file (expand-file-name "persp-save" alan/cache-dir))
  (persp-show-modestring t))

(provide 'ui-config)
;;; ui-config.el ends here.
