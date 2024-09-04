;;; org-config.el  -*- lexical-binding: t -*-

(setq org-pretty-entities t
      org-hide-emphasis-markers t
      org-adapt-indentation t
      org-hide-leading-stars t
      org-ellipsis " ▼ "
      evil-auto-indent nil
      company-idle-delay nil)

(display-line-numbers-mode -1)
(org-indent-mode t)
(evil-org-mode)
(olivetti-mode 1)
(org-superstar-mode 1)
(mixed-pitch-mode 1)

(add-hook 'mixed-pitch-mode-hook
          (lambda () (dolist (face '(org-document-title
                                     org-level-1
                                     org-level-2
                                     org-level-3
                                     org-level-4
                                     org-level-5
                                     org-level-6
                                     org-level-7
                                     org-level-8))
                       (set-face-attribute face nil :family alan/sans-font))))
