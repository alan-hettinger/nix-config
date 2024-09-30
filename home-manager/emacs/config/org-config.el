;;; org-config.el  -*- lexical-binding: t -*-

;;; Commentary:
;; Org configuration. Also contains some general text-mode config.

;;; Code:

(use-package org
  :config
  (setq-local evil-auto-indent nil)
  (display-line-numbers-mode -1)
  :custom
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-adapt-indentation t)
  (org-hide-leading-stars t)
  (org-ellipsis " ▼ ")
  (org-fold-catch-invisible-edits 'show)
  (org-insert-heading-respect-content t)
  (org-directory (expand-file-name "~/Documents/Notes"))
  (org-todo-keywords
   '((sequence
      "TODO(t)"
      "PROJ(p)"
      "LOOP(r)"
      "STRT(s)"
      "WAIT(w)"
      "IDEA(i)"
      "|"
      "DONE(d)"
      "KILL(k)")
     (sequence
      "[ ](T)"
      "[-](S)"
      "[X](D)")))
  :general
  (alan/local-leader
    :keymaps 'org-mode-map
    "@" '(org-cite-insert :which-key "insert citation")
    "." '(consult-org-heading :which-key "goto heading")
    "e" '(org-export-dispatch :which-key "export")
    "f" '(org-footnote-action :which-key "footnote")
    "t" '(org-todo :which-key "to-do")
    "T" '(org-todo-list :which-key "show to-do list")
    "x" '(org-toggle-checkbox :which-key "toggle checkbox")))

(use-package org-indent
  :hook org-mode)

(use-package evil-org
  :hook org-mode
  :custom
  (evil-org-use-additional-insert t))

(use-package org-superstar
  :hook org-mode)

(use-package mixed-pitch
  :hook text-mode
  :config
  (dolist (face '(org-document-title
                  org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8))
    (set-face-attribute face nil :family alan/sans-font)))

(provide 'org-config)

;;; org-config.el ends here.
