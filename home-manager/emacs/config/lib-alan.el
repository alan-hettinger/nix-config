;; lib-alan.el --- local helper functions. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun alan/add-hook-maybe-do (mode function)
  "Add FUNCTION to MODE's hooks. If mode is t, also do hook."
  (progn (when mode (funcall function))
         (add-hook (intern (format "%s-hook" (symbol-name mode)))
                   function)))

(defun alan/add-multiple-hooks (function hooklist)
  "Add FUNCTION as hook for all of HOOKLIST."
  (dolist (hook hooklist) (add-hook hook function)))


(provide 'lib-alan)
;;; lib-alan.el ends here.
