;;; programming-config.el  -*- lexical-binding: t -*-

(global-hl-todo-mode 1)

;; HACK flycheck doesn't recognize elisp packages installed by nix,
;; which causes erroneous undefined variable warnings constantly.
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))
