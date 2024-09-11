;;; programming-config.el  -*- lexical-binding: t -*-

(global-hl-todo-mode 1)

(defun alan/visual-fill-prog ()
  (progn (setq-local visual-fill-column-center-text t
                     visual-fill-column-width 80
                     visual-fill-column-enable-sensible-window-split t
                     visual-fill-column-fringes-outside-margins t)
         (visual-line-fill-column-mode 1)
         (adaptive-wrap-prefix-mode 1)
         (when diff-hl-mode (setq-local diff-hl-side 'right))))
(add-hook 'prog-mode-hook #'alan/visual-fill-prog)

(add-hook 'lispy-mode-hook #'lispyville-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)
;; HACK flycheck doesn't recognize elisp packages installed by nix,
;; which causes erroneous undefined variable warnings constantly.
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))
