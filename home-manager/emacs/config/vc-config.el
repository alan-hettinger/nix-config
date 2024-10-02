;;; vc-config.el --- git, etc config

;;; Commentary:
;;; TODO

;;; Code:

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-diff-refine-hunk t)
  :general
  (alan/vc-map-definer
    "g" '(magit-status :which-key "open magit")
    "s" '(magit-stage-buffer-file :which-key "stage current file")
    "S" '(magit-stage-file :which-key "stage file")
    "t" '(magit-todos-list :which-key "list todos")
    "c" '(magit-commit :which-key "commit")))

(use-package magit-todos
  :hook magit-mode)

(provide 'vc-config)
;;; vc-config.el ends here.
