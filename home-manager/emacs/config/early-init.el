;;; early-init.el --- read during startup  -*- lexical-binding: t -*-

(defun alan/startup-optimization ()
  (progn (setq load-prefer-newer t
               gc-cons-threshold most-positive-fixnum
               ;; gc-cons-threshold (* 100 1024 1024)
               gc-cons-percentage nil
               large-file-warning-threshold (* 100 1024 1024)
               redisplay-skip-fontification-on-input t
               default-frame-alist '((undecorated . t)))
         (setq-default cursor-in-non-selected-windows nil)
         (add-hook 'emacs-startup-hook
                   (lambda () (setq gc-cons-threshold (* 100 1024 1024)
                                    gc-cons-percentage 0.1)))
         (when (boundp 'pgtk-wait-for-event-timeout) ;; save performance on PGTK
           (setq pgtk-wait-for-event-timeout 0.001))))

(defvar alan/theme-initialized-p nil)

(defun alan/re-enable-theme-frame (_frame)
  "Re-enable active theme upon FRAME creation.
Originally from protesilaos' dotfiles on github."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)
    (if (functionp 'alan/enable-theme)
        (alan/enable-theme)
      (enable-theme theme))))

(defun alan/avoid-initial-white-screen ()
  "Avoid a brief white screen on startup.
Also from protesilaos."
  (progn
    (setq mode-line-format nil)
    (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff")
    (setq alan/theme-initialized-p nil)
    (add-hook 'after-make-frame-functions #'alan/re-enable-theme-frame)))

(defun alan/early-ui ()
  (progn (setq inhibit-startup-message t
               frame-resize-pixelwise t
               frame-inhibit-implied-resize t
               window-resize-pixelwise nil
               ring-bell-function 'ignore
               use-dialog-box t
               use-file-dialog nil
               use-short-answers t
               inhibit-splash-screen t
               inhibit-startup-screen t
               inhibit-x-resources t
               inhibit-startup-echo-area-message user-login-name
               inhibit-startup-buffer-menu t)
         (alan/avoid-initial-white-screen)
         (blink-cursor-mode -1)
         (tool-bar-mode -1)
         (tooltip-mode 0) ; make help text appear in echo area
         (menu-bar-mode -1)))

(alan/startup-optimization)
(alan/early-ui)
