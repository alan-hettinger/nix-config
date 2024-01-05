
(provide 'appearance)

(setq doom-font (font-spec :family "mononoki" :size 16.0))
(setq doom-variable-pitch-font (font-spec :family "Source Serif Variable" :height 1.2 :size 16.0))

(use-package! mixed-pitch
  :config
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 1.2)
  )

(use-package! catppuccin-theme
  :config (load-theme 'catppuccin t)
  :init (setq catppuccin-flavor 'macchiato))

(setq doom-theme 'catppuccin)

(setq display-line-numbers-type 'relative)

;; disable title bar in gnome
(setq default-frame-alist '((undecorated . t)))
(setq lsp-headerline-breadcrumb-enable t)
