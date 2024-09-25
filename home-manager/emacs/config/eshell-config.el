;;; package --- eshell-config.el  -*- lexical-binding: t -*-

;;; Commentary:
;; TODO

;;; Code:

(use-package eshell
  :init
  (defun alan/eshell-toggle (arg &optional command)
    "Toggle eshell popup. Shamelessly stolen from Doom."
    (interactive "P")
    (let ((eshell-buffer
           (get-buffer-create
            (format "*eshell-popup:%s*"
                    (if (bound-and-true-p persp-mode)
                        (persp-current-name)
                      "main"))))
          confirm-kill-processes
          current-prefix-arg)
      (when arg
        (when-let (win (get-buffer-window eshell-buffer))
          (delete-window win))
        (when (buffer-live-p eshell-buffer)
          (with-current-buffer eshell-buffer
            (fundamental-mode)
            (erase-buffer))))
      (if-let (win (get-buffer-window eshell-buffer))
          (let (confirm-kill-processes)
            (delete-window win)
            (ignore-errors (kill-buffer eshell-buffer)))
        (with-current-buffer eshell-buffer
          (if (eq major-mode 'eshell-mode)
              (run-hooks 'eshell-mode-hook)
            (eshell-mode)))
        (pop-to-buffer eshell-buffer))))
  :custom
  (eshell-directory-name (expand-file-name "eshell" alan/cache-dir))
  :general
  (alan/leader-keys
    "oe" '(alan/eshell-toggle :which-key "toggle eshell")))

(provide 'eshell-config)
;;; eshell-config.el ends here
