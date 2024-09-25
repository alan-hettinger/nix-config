;;; evil-config.el --- -*- lexical-binding: t -*-

(use-package evil
  :defer nil
  :hook after-init
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config (evil-mode 1)
  (evil-set-initial-state 'eshell-mode 'normal)
  :custom
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :defer nil
  :config (evil-collection-init)
  )

(use-package evil-better-visual-line
  :after evil
  :defer nil
  :config (evil-better-visual-line-on))

(provide 'evil-config)
