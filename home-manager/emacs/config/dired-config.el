;;; dired-config.el --- dired configuration -*- lexical-binding: t -*-

;;; Commentary:
;; TODO

;;; Code:

(require 'config)

;;; Customization group:
(defgroup alan/dired nil
  "Dired-related personal customization."
  :group 'alan-settings)
(defcustom alan/preferred-file-tree nil
  "The file tree that should be used."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Treemacs" treemacs)
                 (const :tag "Dirvish" dirvish))
  :group 'alan/dired)
(defcustom alan/dired-style 'simple
  "Defines the set of Dired configuration to be applied."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Simple" simple)
                 (const :tag "Dirvish" dirvish))
  :group 'alan/dired)


(use-package dired
  ;; :init
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :custom
  (delete-by-moving-to-trash t)
  (trash-directory "~/.local/share/Trash/files/")
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-make-directory-clickable t)
  (dired-mouse-drag-files t)
  (dired-dwim-target t)
  (dired-clean-up-buffers-too t)
  (dired-vc-rename-file t)
  (dired-hide-details-hide-symlink-targets t)
  (dired-kill-when-opening-new-dired-buffer t)
  :general
  (alan/leader-keys
    "d" '(:ignore t :which-key "dired")
    "dd" 'dired-jump)
  :general-config
  (:keymaps 'dired-mode-map
            :states 'normal
            "h" 'dired-up-directory
            "l" 'dired-find-file
            "L" 'dired-find-file-other-window
            "C" 'dired-do-copy
            "D" 'dired-do-delete
            "R" 'dired-do-rename
            "RET" 'dired-open-file)
  (alan/local-leader
    :keymaps 'dired-mode-map
    "s" 'dired-hide-details-mode
    "w" '(dired-toggle-read-only :which-key "dired edit")
    "W" '(wdired-finish-edit :which-key "finish edit")
    "x" '(wdired-abort-changes :which-key "cancel edit")
    "o" '(dired-open-file :which-key "open file")))

(use-package dired-subtree
  :unless (package-installed-p 'dirvish)
  :custom
  (dired-subtree-use-backgrounds nil)
  :general
  (:keymaps 'dired-mode-map
            "<tab>" 'dired-subtree-toggle
            "<backtab>" 'dired-subtree-cycle))

(use-package all-the-icons-dired
  :unless (package-installed-p 'dirvish)
  :hook dired-mode)


;;; treemacs configuration:
;; it's not dired but this is the best place for it
(use-package treemacs
  :init
  (setq treemacs-indentation 1
        treemacs-indentation-string "┃"
        treemacs-width 30
        treemacs-wide-toggle-width 40
        treemacs-user-mode-line-format 'none
        treemacs-text-scale -1)
  :config
  (treemacs-git-mode -1)
  ;; ^ FIXME enabling treemacs git mode causes emacs to hang.
  ;; Traced issue to "treemacs-process-file-events".
  (treemacs-follow-mode 1)
  (display-line-numbers-mode -1))
(use-package treemacs-nerd-icons
  :after treemacs
  :demand t
  :config (treemacs-load-theme "nerd-icons"))


(provide 'dired-config)
;;; dired-config.el ends here.
