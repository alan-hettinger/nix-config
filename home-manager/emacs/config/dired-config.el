;;; dired-config.el -*- lexical-binding: t -*-

(dired-hide-details-mode 1)
(all-the-icons-dired-mode)
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/"
      dired-single-use-magic-buffer t
      dired-single-magic-buffer-name "*dired*"
      dired-auto-revert-buffer #'dired-directory-changed-p
      dired-make-directory-clickable t
      dired-mouse-drag-files t
      dired-dwim-target t
      dired-clean-up-buffers-too t
      dired-vc-rename-file t
      dired-subtree-use-backgrounds nil)

(require 'dired-subtree)
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-single-up-directory
  (kbd "l") 'dired-single-buffer
  (kbd "L") 'dired-find-file-other-window
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "R") 'dired-do-rename
  (kbd "RET") #'dired-open-file
  (kbd "<tab>") 'dired-subtree-toggle
  (kbd "<backtab>") 'dired-subtree-cycle)

(alan/leader-keys
  "d" '(:ignore t :which-key "dired")
  "ds" '(dired-hide-details-mode :which-key "toggle details")
  "dw" '(dired-toggle-read-only :which-key "dired edit")
  "dW" '(wdired-finish-edit :which-key "finish edit")
  "dx" '(wdired-abort-changes :which-key "cancel edit")
  "do" '(dired-open-file :which-key "open file"))
