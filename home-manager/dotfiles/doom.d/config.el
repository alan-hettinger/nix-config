;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Alan Hettinger"
      user-mail-address "alan.hettinger@proton.me")

(load! "appearance")
(after! org
  (load! "writing/thesis"))

(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(use-package! evil-better-visual-line
  :config (evil-better-visual-line-on))
(+global-word-wrap-mode +1)

(setq split-height-threshold nil
      split-width-threshold 40)

(setq company-minimum-prefix-length 3)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq ispell-dictionary "en_US")
(setq +word-wrap-disabled-modes '(vterm-mode))

(setq evil-want-fine-undo t
      )

(map! :leader
      (:prefix "t"
       :desc "toggle modeline"  "m" #'hide-mode-line-mode)
      (:prefix "q"
       :desc "save and quit server-edit frame" "e" #'server-edit
       :desc "abort server-edit frame" "E" #'server-edit-abort)
      )

(map! :i "M-TAB" (cmds! (not (minibufferp)) #'company-complete-common))

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

  (add-hook 'org-mode-hook
	    (setq org-pretty-entities t
		  org-hide-emphasis-markers t
		  org-adapt-indentation t))
  (setq org-ellipsis " ▼ ")
  )

(add-hook 'org-mode-hook
          (lambda () (setq-local company-idle-delay nil)))

(after! org
  (setq org-export-with-section-numbers nil
        org-export-with-toc nil
        ;; org-odt-preferred-output-format docx
        )
  )

(defun org-remove-headlines (backend)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
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
  ;; automatically tangle certain config files on save:
  ;; (defun alan/org-babel-tangle-config ()
  ;;   (when (string-equal (buffer-file-name)
  ;;                       (expand-file-name "./config.org"))
  ;;     (let ((org-confirm-babel-evaluate nil))
  ;;       (org-babel-tangle))))
  ;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'alan/org-babel-tangle-config)))
  )

(use-package! ob-racket
  :after org
  ;;   :config (add-hook 'ob-racket-pre-runtime-library-load-hook
  ;;               #'ob-racket-raco-make-runtime-library)
  )
;; (setq ob-racket-default-lang "sicp")

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

(after! writeroom-mode
  (setq +zen-text-scale 1)
  (setq writeroom-mode-line 't)
  )

(setq olivetti-style nil ;; 'fancy | nil
      olivetti-body-width 70)
(add-hook 'org-mode-hook (lambda () (olivetti-mode 1)))

(map! :leader
      (:prefix "t"
       :desc "toggle Olivetti mode" "o" #'olivetti-mode
       ))

(add-hook 'doom-docs-mode-hook (lambda () (olivetti-mode 'nil)))

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
         vterm-always-compile-module 't
         )
  )

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "R") 'dired-do-rename
  (kbd "RET") #'dired-open-file
  )

(map! :leader
      (:prefix "d"
       :desc "toggle details"  "s" #'dired-hide-details-mode
       :desc "dired edit" "w" #'dired-toggle-read-only
       :desc "finish edit" "W" #'wdired-finish-edit
       :desc "cancel edit" "x" #'wdired-abort-changes
       :desc "open file" "o" #'dired-open-file
       ))

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  (define-key dired-mode-map [remap dired-find-file]
              'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
              'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
              'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-mode-hook 'my-dired-init))

(setq dired-single-use-magic-buffer t
      dired-single-magic-buffer-name "*dired*")

(map! :leader
      (:prefix "d"
       :desc "Open dired"  "d" (function
                                (lambda nil (interactive)
                                  (dired-single-magic-buffer default-directory)))))

(use-package dired-subtree :ensure t)
(evil-define-key 'normal dired-mode-map
  (kbd "<tab>") 'dired-subtree-toggle
  (kbd "<backtab>") 'dired-subtree-cycle
  )
(after! dired
  (setq dired-subtree-use-backgrounds nil))

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

(after! ranger
  (setq ranger-show-hidden t
        ranger-max-parent-width 0.4
        ranger-width-preview 0.4
        ranger-max-preview-size 10
        ranger-dont-show-binary t
        ))
(add-hook 'ranger-mode-hook 'hide-mode-line-mode)

(setq geiser-repl-query-on-kill-p nil)
;; (setq geiser-active-implementations '(guile))
;; (setq geiser-default-implementation '(guile))

(setq lsp-treemacs-errors-position-params `((side . right)))

(after! lua-mode (setq lsp-clients-lua-language-server-bin (executable-find "lua-language-server"))
  (set-lsp-priority! 'lua-language-server 1))



(use-package lsp-mode
  :ensure t)

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

;;; open eshell as if it were a terminal:
;; - invoked via 'emacsclient -a "" -c -e '(server-eshell)'
;; - per https://www.emacswiki.org/emacs/EshellAndEmacsServer
;; (require 'cl)
;; (defun server-eshell ()
;; "Command to be called by emacs-client to start a new shell.
;;
;; A new eshell will be created. When the frame is closed, the buffer is deleted or the shell exits,
;; then hooks will take care that the other actions happen. For example, when the frame is closed,
;; then the buffer will be deleted and the client disconnected."
;; (lexical-let ((buf (eshell t))
;; (client (first server-clients))
;; (frame (selected-frame)))
;; (labels ((close (&optional arg)
;; (when (not (boundp 'cve/recurse))
;; (let ((cve/recurse t))
;; (delete-frame frame)
;; (kill-buffer buf)
;; (server-delete-client client)))))
;; (add-hook 'eshell-exit-hook #'close t t)
;; (add-hook 'delete-frame-functions #'close t t))
;; (delete-other-windows)
;; nil))

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
