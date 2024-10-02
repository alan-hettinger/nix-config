;;; dired-config.el --- dired configuration -*- lexical-binding: t -*-

;;; Commentary:
;; TODO

;;; Code:

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

(use-package dired-single
  :init
  (defun alan/dired-single-open-here ()
    "Open dired-single here"
    (interactive)
    ;; feels like a HACK
    (dired-single-magic-buffer
     (cond (buffer-file-name (file-name-directory (buffer-file-name)))
           (projectile-mode (projectile-project-root))
           (t "~/"))))
  :custom
  (dired-single-use-magic-buffer t)
  (dired-single-magic-buffer-name "*dired*")
  :general
  ([remap dired-jump] #'alan/dired-single-open-here
   [remap dired-find-file] 'dired-single-buffer
   [remap dired-up-directory] 'dired-single-up-directory))

(use-package dired-subtree
  :custom
  (dired-subtree-use-backgrounds nil)
  :general
  (:keymaps 'dired-mode-map
            "<tab>" 'dired-subtree-toggle
            "<backtab>" 'dired-subtree-cycle))

(use-package all-the-icons-dired
  :hook dired-mode)

(provide 'dired-config)
;;; dired-config.el ends here.
