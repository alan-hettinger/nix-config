(setq user-full-name "Alan Hettinger"
      user-mail-address "alan.hettinger@proton.me")

(map! :leader
      (:prefix "t"
	           :desc "toggle modeline"  "m" #'hide-mode-line-mode)
      (:prefix "q"
	           :desc "save and quit server-edit frame" "e" #'server-edit
	           :desc "abort server-edit frame" "E" #'server-edit-abort))

;; rebind lispyville away from bracket keys, per doom docs:
(map! :after (lispy lispyville)
      :map lispy-mode-map-lispy
      ;; unbind individual bracket keys
      "[" nil
      "]" nil
      ;; re-bind commands bound to bracket keys by default
      "M-[" #'lispyville-previous-opening
      "M-]" #'lispyville.next-opening)

(add-hook 'pdf-view-mode-hook
          (lambda () (pdf-view-auto-slice-minor-mode 1)))

(dolist (mode '(org-mode-hook
                markdown-mode-hook
                Info-mode-hook))
  (add-hook mode (lambda () (mixed-pitch-mode 1) ))
  (add-hook mode (lambda () (progn
                              (setq left-margin-width 4)
                              (setq right-margin-width 4)
                              (set-window-buffer nil (current-buffer))))))

(after! org
	    (setq org-startup-folded t)
	    (setq org-directory "~/Documents/")
	    ;; makes info files linkable from org
	    (add-to-list 'org-modules 'ol-info)
	    )

(after! org
	    (setq org-export-with-section-numbers nil
              org-export-with-toc nil
              ;; org-odt-preferred-output-format docx
              )
	    )

(defun org-remove-headlines (backend)
  (org-map-entries (lambda () (delete-region (pos-bol) (pos-eol)))
                   "ignore"))

(add-hook 'org-export-before-processing-functions #'org-remove-headlines)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lua . t)
   (scheme . t)
   (shell . t)
   (racket . t)
   ))

(after! org
	    (require 'org-tempo)
	    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
	    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
	    (add-to-list 'org-structure-template-alist '("lua" . "src lua"))
	    (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
	    (add-to-list 'org-structure-template-alist '("r" . "src racket"))
	    )

(use-package! ob-racket
	          :after org
	          ;;   :config (add-hook 'ob-racket-pre-runtime-library-load-hook
	          ;;               #'ob-racket-raco-make-runtime-library)
	          )

(after! org

	    (setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))
	    (setq org-cite-insert-processor 'citar
              org-cite-follow-processor 'citar
              org-cite-activate-processor 'citar
              citar-bibliography org-cite-global-bibliography
              org-cite-csl-styles-dir "~/Zotero/styles/"
              citar-citeproc-csl-styles-dir "~/Zotero/styles/"
              org-cite-export-processors
              '((latex . (csl "chicago-author-date.csl"))
		        (odt . (csl "chicago-author-date.csl"))
		        (t . (csl "chicago-author-date.csl")))
              )
	    )

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(add-hook 'nov-mode-hook 'variable-pitch-mode)
(setq nov-text-width 80)

(set-popup-rule! "^ ?\\*Treemacs" :ignore t)
(after! treemacs
	    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
	    (treemacs-git-commit-diff-mode 't)
	    (treemacs-git-mode 'extended)
	    (treemacs-indent-guide-mode 't)
	    (hide-mode-line-mode 't)
	    (setq treemacs-indentation 1
              treemacs-indentation-string "┃"
              treemacs-width 25
              treemacs-wide-toggle-width 40
              treemacs-text-scale 1
              ))

(remove-hook 'vterm-mode-hook #'hide-mode-line-mode)
(after! vterm
	    (setq  vterm-shell "zsh"
               vterm-copy-exclude-prompt 't
               vterm-buffer-name-string "vterm %s"
               vterm-always-compile-module 't))

(map! :leader
      (:prefix "d"
	           :desc "Open dired"  "d" (function
					                    (lambda nil (interactive)
					                      (dired-single-magic-buffer default-directory)))))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "ristretto")
                                ("docx" . "libreoffice")
                                ("docx" . "libreoffice")
                                ("ods" . "libreoffice")
                                ("odt" . "libreoffice")
                                ("xlsx" . "libreoffice")
                                ("pptx" . "libreoffice")
                                ("odp" . "libreoffice")
                                )))

(setq lsp-treemacs-errors-position-params `((side . right)))

(after! lua-mode (setq lsp-clients-lua-language-server-bin (executable-find "lua-language-server"))
	    (set-lsp-priority! 'lua-language-server 1))

;;; eshell aliases and functions:
(setq alan/eshell-aliases
      '((d . dired)
        (l . (lambda () (eshell/ls '-la)))
        (o . find-file)
        (open . find-file)
        (g . magit)
        ))
(mapc (lambda (alias)
        (defalias (car alias) (cdr alias))) alan/eshell-aliases)

(defun eshell/edit-thesis ()
  (progn (eshell/cd "~/Documents/Thesis")
         (find-file "thesis-draft.org")
         (find-file-other-window "~/Documents/Notes/thesis/thesis-notes.org")
         (+workspace/rename "thesis")
         (treemacs-do-switch-workspace "Perspective thesis")
         ))

(defun coinflip () (if (= 1 (random 2)) "heads" "tails"))
(defun dice-roll (x) (+ 1 (random x)))
