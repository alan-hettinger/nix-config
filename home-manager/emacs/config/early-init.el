;;; early-init.el --- read during startup  -*- lexical-binding: t -*-

(defun alan/startup-optimization ()
  (progn (setq load-prefer-newer t)))

(defun alan/early-ui ()
  ())
