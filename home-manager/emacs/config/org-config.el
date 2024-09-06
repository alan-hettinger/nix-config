;;; org-config.el  -*- lexical-binding: t -*-

(setq org-pretty-entities t
      org-hide-emphasis-markers t
      org-adapt-indentation t
      org-hide-leading-stars t
      org-ellipsis " ▼ "
      evil-auto-indent nil
      company-idle-delay nil
      org-fold-catch-invisible-edits 'show
      org-insert-heading-respect-content t
      org-directory (expand-file-name "~/Documents/Notes"))

(setq org-todo-keywords
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

(display-line-numbers-mode -1)
(org-indent-mode t)
(evil-org-mode)
(visual-fill-column-mode 1)
(setq-local visual-fill-column-width 120
            visual-fill-column-center-text t)
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

;; Keybindings:
(alan/local-leader
  :keymaps 'org-mode-map
  "@" '(org-cite-insert :which-key "insert citation")
  "." '(consult-org-heading :which-key "goto heading")
  "e" '(org-export-dispatch :which-key "export")
  "f" '(org-footnote-action :which-key "footnote")
  "t" '(org-todo :which-key "to-do")
  "T" '(org-todo-list :which-key "show to-do list")
  "x" '(org-toggle-checkbox :which-key "toggle checkbox")
  )
