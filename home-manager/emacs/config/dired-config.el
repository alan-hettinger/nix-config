(dired-hide-details-mode 1)
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/"
      dired-single-use-magic-buffer t
      dired-single-magic-buffer-name "*dired*"
      dired-subtree-use-backgrounds nil)

(require 'dired-subtree)
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "R") 'dired-do-rename
  (kbd "RET") #'dired-open-file
  (kbd "<tab>") 'dired-subtree-toggle
  (kbd "<backtab>") 'dired-subtree-cycle)

(alan/leader-keys
  "d" '(:ignore t :which-key "dired")
  "dd" '(dired-single-magic-buffer :which-key "open dired here")
  "ds" '(dired-hide-details-mode :which-key "toggle details")
  "dw" '(dired-toggle-read-only :which-key "dired edit")
  "dW" '(wdired-finish-edit :which-key "finish edit")
  "dx" '(wdired-abort-changes :which-key "cancel edit")
  "do" '(dired-open-file :which-key "open file"))

(define-key dired-mode-map [remap dired-find-file]
            'dired-single-buffer)
(define-key dired-mode-map [remap dired-mouse-find-file-other-window]
            'dired-single-buffer-mouse)
(define-key dired-mode-map [remap dired-up-directory]
            'dired-single-up-directory)
