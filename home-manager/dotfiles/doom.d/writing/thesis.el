;;; thesis.el -*- lexical-binding: t; -*-
;; configuration that is specifically useful for my MA thesis, written in org-mode
(after! org

  ;; automatically export thesis to pdf on save:
  (defun alan/export-thesis-draft ()
    (let* ((main-filename-str "thesis-draft")
           (date-format-str (format-time-string "%Y-%m-%d"))
           (target-path "./drafts/")
           (target-filename (format "%s%s-%s.tex" target-path date-format-str main-filename-str)))
      (when (string-equal (buffer-file-name) (expand-file-name "./thesis-draft.org"))
        (progn
          ;; create the pdf:
          (org-export-to-file 'latex target-filename
            nil              ;; export asynchronously
            nil nil nil nil  ;; bunch of args I don't care about
            #'org-latex-compile)
          ;; clean up the latex file:
          (delete-file target-filename)))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'alan/export-thesis-draft)))

  ;; handle the bibliography:
  (setq org-cite-global-bibliography '("~/Documents/Thesis/zotero-lib.bib"))

  )
