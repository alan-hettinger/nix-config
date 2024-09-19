;;; package --- eshell-config.el  -*- lexical-binding: t -*-

;;; Commentary:
;; TODO

;;; Code:

(use-package eshell
  :custom
  (eshell-directory-name (expand-file-name "eshell" alan/cache-dir)))

(provide 'eshell-config)
;;; eshell-config.el ends here
