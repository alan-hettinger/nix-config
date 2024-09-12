;; alternate-modeline.el -*- lexical-binding: t -*-

(defun alan/mode-line--show-only-on-selected (construct)
  (when (mode-line-window-selected-p) construct))
(defvar alan/modeline--text-scale-factor 1.0
  "For making sure text aligns properly")

;; (defun alan/mode-line--fill-to-center (reserve face)
;;   (when alan/modeline--text-scale-factor
;;     (setq reserve (* alan/modeline--text-scale-factor reserve)))
;;   (propertize " "
;;               'display `((space :align-to (- (+ center (.5 . right-margin))
;;                                              ,reserve
;;                                              (.5 . left-margin))))
;;               'face face))
(defun alan/mode-line-fill-center (face reserve)
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ center
                                                (.5 . right-margin))
                                             ,reserve
                                             (.5 . left-margin))))
              'face face))

(defconst RIGHT_PADDING 5)
(defun alan/mode-line-fill-right (face reserve)
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun reserve-left/middle ()
  ;; FIXME dividing by 2 should align center but 1.5 looks better
  (/ (length (format-mode-line alan/mode-line-center)) 1.5))
(defun reserve-middle/right ()
  (+ RIGHT_PADDING (length (format-mode-line alan/mode-line-right))))

(defface alan/mode-line--buffer-face
  `((t :inherit bold :foreground ,(catppuccin-color 'blue)))
  "test docstring")
(defface alan/mode-line--buffer-modified-face
  `((t :inherit bold :foreground ,(catppuccin-color 'yellow)))
  "test docstring")
(defface alan/mode-line--buffer-ro-face
  `((t :inherit bold :foreground ,(catppuccin-color 'subtext1)))
  "fake docstring")

(defun alan/mode-line--format-title (buffer-str)
  (propertize
   buffer-str
   'face (cond ((buffer-modified-p)
                'alan/mode-line--buffer-modified-face)
               ((eval buffer-read-only)
                'alan/mode-line--buffer-ro-face)
               ((eval t)
                'alan/mode-line--buffer-face))))

(defun alan/modeline--buffer-name ()
  (alan/mode-line--format-title (buffer-name)))

(defun alan/modeline--major-mode ()
  (capitalize (symbol-name major-mode)))

(defvar-local alan/modeline-mode-icon
    (format "%s " (nerd-icons-icon-for-buffer)))

(defvar-local alan/mode-line-buffer-title
    '(:eval (alan/modeline--buffer-name)))

(defvar-local alan/mode-line-major-mode-name
    `(:eval ,(alan/mode-line--show-only-on-selected
              (alan/modeline--major-mode))))

(put 'alan/mode-line-buffer-title 'risky-local-variable t)
(put 'alan/mode-line-major-mode-name 'risky-local-variable t)
(put 'alan/modeline-mode-icon 'risky-local-variable t)

(setq alan/mode-line-left
      '("%e"
        (:eval evil-mode-line-tag)
        mode-line-position))
(setq alan/mode-line-center
      '((:eval alan/modeline-mode-icon)
        alan/mode-line-buffer-title))
(setq alan/mode-line-right
      'alan/mode-line-major-mode-name)

(setq-default mode-line-format
              (list
               alan/mode-line-left
               '(:eval (alan/mode-line-fill-center 'mode-line
                                                   (reserve-left/middle)))
               alan/mode-line-center
               '(:eval (alan/mode-line-fill-right 'mode-line
                                                  (reserve-middle/right)))
               alan/mode-line-right))
