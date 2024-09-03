
;; set some basic modes:
(global-display-line-numbers-mode 0)
(setq display-line-numbers-type t
      inhibit-startup-message t)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 20)
(global-visual-line-mode 1)

(defun my/disable-scroll-bars (frame)
  "Always hide scroll bars. Useful with emacs server."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; load packages that should always be on
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

(solaire-global-mode +1)
