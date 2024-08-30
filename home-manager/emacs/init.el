(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(doom-modeline-mode 1)
(company-mode 1)

(setq catppuccin-flavor 'macchiato
      catppuccin-italic-blockquotes 'nil
      catppuccin-highlight-matches t
      catppuccin-italic-variables 'nil)
(load-theme 'catppuccin t)

;; FIXME:
;; (let ((new-bg (catppuccin-get-color 'crust)))
;;   (set-face-attribute 'hl-line nil :background new-bg))

(setq enable-recursive-minibuffers t
      split-height-threshold nil
      split-width-threshold 40
      company-minimum-prefix-length 3
      default-frame-alist '((undecorated . t))
      frame-title-format '("%b"))

(setq doom-modeline-enable-word-count t
      doom-modeline-major-mode-icon t
      doom-modeline-persp-name t
      doom-modeline-height 35
      doom-modeline-icon t
      doom-modeline-modal-modern-icon nil
      doom-modeline-buffer-modification-icon t
      doom-modeline-highlight-modified-buffer-name nil)
(remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
(remove-hook 'doom-modeline-mode-hook #'column-number-mode)

  (add-hook 'org-mode-hook
	    (setq org-pretty-entities t
		  org-hide-emphasis-markers t
		  org-adapt-indentation t))
  (setq org-ellipsis " ▼ ")

(require 'evil)
(evil-mode 1)
(setq evil-want-fine-undo t
      evil-undo-system 'undo-redo)

(vertico-mode 1)
(savehist-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
