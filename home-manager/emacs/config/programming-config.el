;;; programming-config.el  -*- lexical-binding: t -*-

(global-hl-todo-mode 1)

(defun alan/visual-fill-prog ()
  (progn (setq-local visual-fill-column-center-text t
                     visual-fill-column-width 80
                     visual-fill-column-enable-sensible-window-split t
                     visual-fill-column-fringes-outside-margins t)
         (visual-line-fill-column-mode 1)
         (adaptive-wrap-prefix-mode 1)
         (when (and (boundp 'diff-hl-mode) diff-hl-mode) (setq-local diff-hl-side 'right))))
(add-hook 'prog-mode-hook #'alan/visual-fill-prog)

(add-hook 'lispy-mode-hook #'lispyville-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)
;; HACK flycheck doesn't recognize elisp packages installed by nix,
;; which causes erroneous undefined variable warnings constantly.
;; (add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))

(use-package nix-mode
  :init
  (add-hook 'nix-mode-hook #'eglot-ensure))

;; nix-mode keybinds:
(alan/local-leader
  :keymaps 'nix-mode-map
  "s" #'nix-search
  "f" #'(:ignore t :which-key "flake")
  "fu" #'(nix-flake-update :which-key "update"))

(alan/local-leader
  :keymaps 'prog-mode-map
  "c" #'consult-flycheck)

(provide 'programming-config)
