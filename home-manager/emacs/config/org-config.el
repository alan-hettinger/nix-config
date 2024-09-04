;;; org-config.el  -*- lexical-binding: t -*-

(setq org-pretty-entities t
      org-hide-emphasis-markers t
      org-adapt-indentation t
      org-hide-leading-stars t
      org-ellipsis " ▼ "
      company-idle-delay nil)

(display-line-numbers-mode -1)
(org-indent-mode t)
(evil-org-mode)
(olivetti-mode 1)
;; (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
