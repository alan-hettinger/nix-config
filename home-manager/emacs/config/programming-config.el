;;; programming-config.el --- programming config  -*- lexical-binding: t -*-

;;; Commentary:
;; TODO keybindings to rotate bools etc
;; TODO Langs:
;;      - scheme
;;      - common-lisp
;;      - racket
;;      - rust
;;      - python?
;;		- js? and html+css

;;; Code:

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 1)
  :general-config
  (alan/lsp-map-definer
    :keymaps 'eglot-mode-map
    "a" #'eglot-code-actions
    "r" #'eglot-rename
    "j" #'consult-eglot-symbols))

(use-package consult-eglot
  :general
  (:keymaps 'eglot-mode-map
            [remap xref-find-apropos] #'consult-eglot-symbols))
(use-package flycheck-eglot
  :hook (eglot-managed-mode . flycheck-eglot-mode))

(use-package hl-todo
  :hook prog-mode)

(use-package apheleia
  :hook prog-mode
  :config
  (push '(nixfmt . ("alejandra")) apheleia-formatters))

(use-package lispy
  :hook (emacs-lisp-mode lisp-mode scheme-mode racket-mode))
(use-package lispyville
  :hook lispy-mode)

(use-package nix-mode
  :init
  (add-hook 'nix-mode-hook #'eglot-ensure)
  :general-config
  (alan/local-leader
    :keymaps 'nix-mode-map
    "s" #'nix-search
    "f" #'(:ignore t :which-key "flake")
    "fu" #'(nix-flake-update :which-key "update")))

(alan/local-leader
  :keymaps 'prog-mode-map
  "c" #'consult-flycheck
  "d" #'evil-goto-definition)

;; lua-mode:
(use-package lua-mode
  :after 'eglot
  :init
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))
  :custom
  (lua-indent-level 2))

(provide 'programming-config)
;;; programming-config.el ends here.
