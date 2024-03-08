;;; thesis.el -*- lexical-binding: t; -*-
;; configuration that is specifically useful for my MA thesis

(let* ((thesis-file-name "thesis-draft")
       (thesis-path "~/Documents/Thesis")
       (buffer-is-thesis (lambda () (string-equal
                                     (buffer-file-name)
                                     (expand-file-name
                                      (format "./%s.org"
                                              thesis-file-name))))))

  ;;; automatically export thesis to pdf on save:
  (defun alan/export-thesis-draft ()
    (let* ((date-format-str (format-time-string "%Y-%m-%d"))
           (target-path "./drafts/")
           (target-filename (format "%s%s-%s.tex"
                                    target-path
                                    date-format-str
                                    thesis-file-name)))
      (when (funcall buffer-is-thesis)
        ;; create the pdf:
        (org-export-to-file 'latex target-filename
          nil             ;; export asynchronously
          nil nil nil nil ;; bunch of args I don't care about
          #'org-latex-compile)
        ;; clean up the latex file:
        (delete-file target-filename))))
  (add-hook 'org-mode-hook
            (lambda () (add-hook 'after-save-hook #'alan/export-thesis-draft)))

  ;;; TODO various other thesis setup:
  (defun alan/thesis-setup () )

  ;;; handle the bibliography:
  (setq org-cite-global-bibliography (list (format "%s/zotero-lib.bib"
                                                   thesis-path)))

  )
