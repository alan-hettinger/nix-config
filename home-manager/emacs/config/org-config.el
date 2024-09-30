;;; org-config.el  -*- lexical-binding: t -*-

;;; Commentary:
;; Org configuration. Also contains some general text-mode config.

;;; Code:

(use-package org
  :config
  (setq-local evil-auto-indent nil)
  (display-line-numbers-mode -1)

  ;; exporting:
  (defun org-remove-headlines (backend)
    (org-map-entries (lambda () (delete-region (pos-bol) (pos-eol)))
                     "ignore"))
  (add-hook 'org-export-before-processing-functions #'org-remove-headlines)


  ;; helper functions for keybinds:
  (defun alan/org-shift-return (&optional arg)
    "Insert literal newline or DWIM in tables."
    (interactive "p")
    (if (org-at-table-p)
        (org-table-copy-down arg)
      (org-return nil arg)))

  :custom
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-adapt-indentation t)
  (org-hide-leading-stars t)
  (org-ellipsis " ▼ ")
  (org-fold-catch-invisible-edits 'show)
  (org-insert-heading-respect-content t)
  (org-directory (expand-file-name "~/Documents/Notes"))
  (org-persist-directory (expand-file-name "org/persist/" alan/cache-dir))
  (org-publish-timestamp-directory (expand-file-name "org/timestamps/"))
  (org-list-allow-alphabetical t)
  (org-enforce-todo-dependencies t)
  (org-fontify-done-headlines t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-startup-indented t)
  (org-startup-folded t)
  (org-special-ctrl-a/e t)
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

  ;; exporting:
  (org-export-with-section-numbers nil)
  (org-export-with-toc nil)
  (org-latex-packages-alist '(("margin=2cm" "geometry" nil)))

  :general
  (:keymaps 'org-mode-map
            "S-RET" #'alan/org-shift-return)
  (alan/local-leader
    :keymaps 'org-mode-map
    "@" '(org-cite-insert :which-key "insert citation")
    "." '(consult-org-heading :which-key "goto heading")
    "/" #'consult-org-agenda
    "e" '(org-export-dispatch :which-key "export")
    "f" '(org-footnote-action :which-key "footnote")
    "t" '(org-todo :which-key "to-do")
    "T" '(org-todo-list :which-key "show to-do list")
    "x" '(org-toggle-checkbox :which-key "toggle checkbox")))

(use-package citar
  :hook (org-mode . citar-capf-setup)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (org-cite-csl-styles-dir "~/Zotero/styles/")
  (citar-citeproc-csl-styles-dir "~/Zotero/styles/")
  (org-cite-export-processors
   '((latex . (csl "chicago-author-date.csl"))
	 (odt . (csl "chicago-author-date.csl"))
	 (t . (csl "chicago-author-date.csl")))))

(use-package org-indent
  :hook org-mode)

(use-package evil-org
  :hook org-mode
  :config
  (evil-org-set-key-theme
   '(;; The defaults:
     navigation insert textobjects additional calendar
     ;; `return' makes pressing RET consistent with pressing o
     return))
  :custom
  (evil-org-use-additional-insert t)
  (evil-org-retain-visual-state-on-shift t))

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

;; Exporting:
(use-package ox-pandoc
  :after org
  :init
  (add-to-list 'org-export-backends 'pandoc))

(provide 'org-config)

;;; org-config.el ends here.
