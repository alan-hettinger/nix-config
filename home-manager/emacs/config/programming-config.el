;;; programming-config.el  -*- lexical-binding: t -*-

(global-hl-todo-mode 1)

(add-hook 'highlight-indent-guides-mode-hook
          (lambda () (setq highlight-indent-guides-method 'bitmap
                           highlight-indent-guides-responsive 'top)))
;; (add-hook 'prog-mode-hook #'highlight-indent-guides-mode) ;; FIXME this causes some kind of error with completion

(add-hook 'lispy-mode-hook #'lispyville-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)
;; HACK flycheck doesn't recognize elisp packages installed by nix,
;; which causes erroneous undefined variable warnings constantly.
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))
