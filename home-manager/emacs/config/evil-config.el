;;; evil-config.el --- -*- lexical-binding: t -*-

(use-package evil
  :defer nil
  :hook after-init
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'eshell-mode 'normal)
  ;; set these here because they aren't defined as custom:
  (setq evil-visual-state-cursor 'hollow
        evil-emacs-state-cursor 'hbar)
  :custom
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-redo)
  (evil-kbd-macro-suppress-motion-error t)
  (evil-visual-update-x-selection-p nil)
  (evil-visual-state-cursor 'hollow))

(use-package evil-collection
  :after evil
  :defer nil
  :init (evil-collection-init)
  :custom
  (evil-collection-key-blacklist '("SPC")))

(use-package evil-better-visual-line
  :after evil
  :ensure t
  :defer nil
  :config (evil-better-visual-line-on))

(use-package evil-args)

(provide 'evil-config)
