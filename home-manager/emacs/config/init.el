
;; basic UI setup:
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type t) ;; t | 'relative
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(doom-modeline-mode 1)
(global-company-mode 1)
(which-key-mode)

;; start tree-sitter:
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(set-face-attribute 'default nil :font "mononoki" :height 160)
(require 'mixed-pitch)
(setq mixed-pitch-set-height t)

(electric-pair-mode)

(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(setq catppuccin-flavor 'macchiato
      catppuccin-italic-blockquotes nil
      catppuccin-highlight-matches t
      catppuccin-italic-variables nil)
(load-theme 'catppuccin t)

(let ((new-bg (catppuccin-get-color 'crust)))
  (set-face-background 'hl-line new-bg))

(setq enable-recursive-minibuffers t
      split-height-threshold nil
      split-width-threshold 40
      company-minimum-prefix-length 3
      default-frame-alist '((undecorated . t))
      frame-title-format '("%b"))

(setq doom-modeline-enable-word-count t
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
(remove-hook 'doom-modeline-mode-hook #'column-number-mode)

(add-hook 'org-mode-hook
	  (setq org-pretty-entities t
		org-hide-emphasis-markers t
		org-adapt-indentation t
		org-hide-leading-stars t
		org-ellipsis " ▼ "))
(add-hook 'org-mode-hook (display-line-numbers-mode -1))
(add-hook 'org-mode-hook #'org-indent-mode)

(setq evil-want-keybinding nil
      evil-want-fine-undo t
      evil-undo-system 'undo-redo)
(evil-collection-init)
(evil-mode 1)
(add-hook 'org-mode-hook 'evil-org-mode)
;; (evil-org-set-key-theme '(navigation insert textobjects additional calendar))

(add-hook 'evil-mode-hook #'evil-better-visual-line-on)

(vertico-mode 1)
(savehist-mode 1)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles partial-completion))))
(require 'marginalia)
(marginalia-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(require 'apheleia)
(apheleia-global-mode +1)
(push '(nixfmt . ("alejandra")) apheleia-formatters)

(setq olivetti-style 'fancy
      olivetti-body-width 90)
(add-hook 'org-mode-hook (lambda () (olivetti-mode 1)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))
(add-hook 'nix-mode-hook #'eglot-ensure)

(with-eval-after-load 'treemacs
  (progn
    (setq treemacs-indentation 1
	  treemacs-indentation-string "┃"
	  treemacs-width 25
	  treemacs-wide-toggle-width 40
	  treemacs-text-scale 1)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
    (treemacs-git-commit-diff-mode t)
    (treemacs-git-mode 'extended)
    (treemacs-indent-guide-mode t)
    ;; (hide-mode-line-mode t)
    ))
