;;; ui-config.el --- appearance and basic behavior  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; some basic settings:
(global-display-line-numbers-mode 0)
(setq-default display-line-numbers-width 3
              display-line-numbers-widen t)
(require 'display-line-numbers)
(setq display-line-numbers-type t
      inhibit-startup-message t
      default-frame-alist '((undecorated . t))
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      window-resize-pixelwise nil
      echo-keystrokes 0.02
      auto-save-no-message t)

;; Some standard modes:
;; (scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode 0) ; make help text appear in echo area
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

(defun alan/disable-scroll-bars (frame)
  "Hide scroll bars on creating any FRAME. Useful with Emacs server."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(defun alan/hide-minibuffer-scroll-bar (frame)
  "Hide the minibuffer scroll bar in FRAME."
  (set-window-scroll-bars (minibuffer-window frame) 0 nil 0 nil t))
(add-hook 'after-make-frame-functions
          ;; 'alan/disable-scroll-bars
          'alan/hide-minibuffer-scroll-bar)

;; load packages that should always be on
(require 'doom-modeline)
(defun ui/doom-modeline-setup ()
  (progn (setq doom-modeline-enable-word-count t
	           doom-modeline-major-mode-icon t
	           doom-modeline-persp-name t
	           doom-modeline-height 30
	           doom-modeline-icon t
	           doom-modeline-modal-modern-icon nil
	           doom-modeline-buffer-modification-icon t
	           doom-modeline-highlight-modified-buffer-name nil
	           doom-modeline-hud t
	           doom-modeline-continuous-word-count-modes '(markdown-mode org-mode)
	           doom-modeline-buffer-encoding nil
	           doom-modeline-project-detection 'project)
	     (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
	     (remove-hook 'doom-modeline-mode-hook #'column-number-mode)))
(add-hook 'doom-modeline-mode-hook #'ui/doom-modeline-setup)
(doom-modeline-mode 1)

(require 'solaire-mode)
(solaire-global-mode +1)

(when (functionp 'golden-ratio)
  (progn (setq golden-ratio-auto-scale t
               golden-ratio-adjust-factor .8
               golden-ratio-wide-adjust-factor .8)
         (golden-ratio-mode 1)))

;; highlight number literals:
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'highlight-numbers-mode-hook
          (lambda ()
            (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>")))

(provide 'ui-config)
;;; ui-config.el ends here.
