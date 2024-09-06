;;; programming-config.el  -*- lexical-binding: t -*-

(global-hl-todo-mode 1)

(add-hook 'highlight-indent-guides-mode-hook
          (lambda () (setq highlight-indent-guides-method 'bitmap
                           highlight-indent-guides-responsive 'top)))
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

;; HACK flycheck doesn't recognize elisp packages installed by nix,
;; which causes erroneous undefined variable warnings constantly.
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))
