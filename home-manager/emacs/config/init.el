
;; FIXME:
(defun my/load-hack (file-name)
  "Load .el files from .emacs.d or equivalent.
   Needed because emacs can't find those files, likely because of symlinks related to nixos."
  (load-file (format "%s%s.el" user-emacs-directory file-name)))

;; basic UI setup:
(add-hook 'after-init-hook (lambda () (my/load-hack "ui-config")))

;; load completion:
(add-hook 'after-init-hook (lambda () (my/load-hack "completion-config")))

;; start tree-sitter:
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(set-face-attribute 'default nil :font "mononoki" :height 160)
(require 'mixed-pitch)
(setq mixed-pitch-set-height t)


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
      default-frame-alist '((undecorated . t))
      frame-title-format '("%b"))

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
