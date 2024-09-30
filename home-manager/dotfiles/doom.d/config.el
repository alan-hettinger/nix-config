(setq user-full-name "Alan Hettinger"
      user-mail-address "alan.hettinger@proton.me")

(add-hook 'pdf-view-mode-hook
          (lambda () (pdf-view-auto-slice-minor-mode 1)))


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
