;;; ui-config.el  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; some basic settings:
(global-display-line-numbers-mode 0)
(setq-default display-line-numbers-width 3
              display-line-numbers-widen t
              truncate-partial-width-windows nil)
(require 'display-line-numbers)
(setq display-line-numbers-type t
      echo-keystrokes 0.02
      auto-save-no-message t
      help-window-select t
      eval-expression-print-length nil
      kill-do-not-save-duplicates t
      scroll-error-top-bottom t)

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
(add-hook 'global-hl-line-mode-hook
          (lambda () (setq hl-line-sticky-flag nil)))
(global-hl-line-mode 1)

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

;; load packages that should always be on
(require 'doom-modeline)
(defun alan/doom-modeline-setup ()
  (let ((mode-line-font-height (round (* 1.0 alan/font-size))))
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
	             doom-modeline-project-detection 'project
                 mode-line-right-align-edge 'right-fringe)
	       (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
	       (remove-hook 'doom-modeline-mode-hook #'column-number-mode)
           (if (facep 'mode-line-active)
               (set-face-attribute 'mode-line-active nil
                                   :family alan/mono-font
                                   :height mode-line-font-height)
             (set-face-attribute 'mode-line nil
                                 :family alan/mono-font
                                 :height mode-line-font-height))
           (set-face-attribute 'mode-line-inactive nil
                               :family alan/mono-font
                               :height mode-line-font-height))))
(add-hook 'doom-modeline-mode-hook #'alan/doom-modeline-setup)
(doom-modeline-mode 1)

(require 'solaire-mode)
(solaire-global-mode +1)

;; highlight number literals:
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'highlight-numbers-mode-hook
          (lambda ()
            (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>")))

(provide 'ui-config)
;;; ui-config.el ends here.
