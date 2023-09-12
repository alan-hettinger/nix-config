;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Alan Hettinger"
      user-mail-address "alan.hettinger@proton.me")

(setq doom-font (font-spec :family "mononoki" :size 21))
(setq doom-variable-pitch-font (font-spec :family "Source Serif Pro" :height 1.2 :size 22))

(use-package! mixed-pitch
  :config
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 1.2)
  )

;; (load-theme 'catppuccin t t)
;; (setq catppuccin-flavor 'macchiato)
(use-package! catppuccin-theme
  :config (load-theme 'catppuccin t)
  :init (setq catppuccin-flavor 'macchiato))
;; (catppuccin-reload)
(setq doom-theme 'catppuccin)
;; (setq doom-themes-padded-modeline t)

;; (setq fancy-splash-image "~/.doom.d/Emacs-logo.png")

(setq display-line-numbers-type 'relative)

;; disable title bar in gnome
(setq default-frame-alist '((undecorated . t)))
(setq lsp-headerline-breadcrumb-enable t)

(after! doom-modeline
  (setq doom-modeline-enable-word-count t
        doom-modeline-major-mode-icon t
        doom-modeline-persp-name t
        doom-modeline-height 25
        doom-modeline-icon t
        )
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (remove-hook 'doom-modeline-mode-hook #'column-number-mode)
  (line-number-mode -1)
)

(defun doom-custom-banner ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'` ' '`---^`---'`---'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32))) "\n"))
       'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'doom-custom-banner)

(dolist (mode '(org-mode-hook
                vterm-mode-hook
                eshell-mode-hook
                markdown-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(add-hook 'pdf-view-mode-hook
          (lambda () (pdf-view-auto-slice-minor-mode 1)))

;; (setq company-global-modes '(not org-mode, not markdown-mode not eshell-mode))

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
  (setq org-directory "~/Documents/Notes/org/")
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

(add-hook 'org-export-before-processing-hook #'org-remove-headlines)

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
  (defun alan/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "./config.org"))
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'alan/org-babel-tangle-config)))
)

(use-package! ob-racket
  :after org
;;   :config (add-hook 'ob-racket-pre-runtime-library-load-hook
;;               #'ob-racket-raco-make-runtime-library)
  )
;; (setq ob-racket-default-lang "sicp")

(after! org

  (setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))
  (setq org-cite-global-bibliography '("~/Documents/Thesis/zotero-lib.bib")
	org-cite-insert-processor 'citar
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

;; (setq +zen-mixed-pitch-modes 'nil)

(setq olivetti-style 'fancy
      olivetti-body-width 70)
(add-hook 'org-mode-hook (lambda () (olivetti-mode 1)))

(map! :leader
      (:prefix "t"
               :desc "toggle Olivetti mode" "o" #'olivetti-mode
                ))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(add-hook 'nov-mode-hook 'variable-pitch-mode)
(setq nov-text-width 80)

(set-popup-rule! "^ ?\\*Treemacs" :ignore t)
(after! treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (treemacs-git-commit-diff-mode 't)
  (treemacs-git-mode 'extended)
  (treemacs-indent-guide-mode 't)
  (setq treemacs-indentation 1
        treemacs-indentation-string "┃"
        treemacs-width 25
        treemacs-wide-toggle-width 40
        treemacs-text-scale 1
        ;; treemacs-persist-file "~/.doom.d/conf/treemacs-persist.org"
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

  ;; ;;          doesn't work:
  ;; (setq
  ;;  lsp-clients-lua-language-server-bin (executable-find "lua-language-server")
  ;;       lsp-clients-emmy-lua-jar-path "~/.local.share/doom/lsp/emmy-lua/EmmyLua-LS-all.jar"
  ;;       lsp-clients-emmy-lua-java-path "~/.nix-profile/bin/java"
  ;;               )

;; ;;            doesn't work but different
;; (defun lslua-init ()
;;   "updates the lua lsp variable and runs lsp"
;;   (interactive)
;;   (setq lsp-clients-lua-language-server-bin (executable-find "lua-language-server"))
;;   (lsp)
;;   )
;; (add-hook 'lua-mode-hook #'lslua-init)

;; ;;           works?!
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
