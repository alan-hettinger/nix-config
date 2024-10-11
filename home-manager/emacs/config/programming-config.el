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
  :after (consult eglot)
  :general
  (:keymaps 'eglot-mode-map
            [remap xref-find-apropos] #'consult-eglot-symbols))

(use-package flycheck-eglot
  :after flycheck
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
(use-package macrostep
  :general
  (alan/local-leader
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "m" #'macrostep-expand))

(use-package nix-mode
  :init
  (add-hook 'nix-mode-hook #'eglot-ensure)
  :general-config
  (alan/local-leader
    :keymaps 'nix-mode-map
    "s" #'nix-search
    "f" #'(:ignore t :which-key "flake")
    "fu" #'(nix-flake-update :which-key "update")))

;; lua-mode:
(use-package lua-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))
  :custom
  (lua-indent-level 2))

;;; Web:
(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.phtml\\'"
         "\\.php\\'"
         "\\.tpl\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"))


(provide 'programming-config)
;;; programming-config.el ends here.
