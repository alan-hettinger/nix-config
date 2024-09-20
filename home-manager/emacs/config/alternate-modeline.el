;;; alternate-modeline.el --- personal modeline -*- lexical-binding: t -*-

;;; Commentary:
;; TODO: move most state into local vars that are updated on hooks
;; 		- This reduces the number of times functions are reevaluated

;;; Code:

(define-minor-mode alan/mode-line-mode
  "Apply my custom mode line."
  :init-value nil
  :interactive t
  :global t
  :group 'alan-mode-line
  (if (not alan/mode-line-mode)
      ;; Entry:
      (progn (alan-mode-line/save-previous-format))
    ;; Exit:
    (progn (when (boundp 'alan-mode-line/prev-format)
             (alan-mode-line/restore-previous-format)))
    ))

(defvar-local alan-mode-line/prev-format nil
  "Previous mode line format, if any.")
(defun alan-mode-line/save-previous-format ()
  "Save previous mode line format."
  (unless alan/mode-line-mode
    (setq-local alan-mode-line/prev-format mode-line-format)))
(defun alan-mode-line/restore-previous-format ()
  "Restore previous mode line format."
  (setq-local mode-line-format alan-mode-line/prev-format))

(defgroup alan-mode-line nil
  "Custom mode line."
  :group 'mode-line)
(defgroup alan-mode-line/faces nil
  "Faces for my custom mode line."
  :group 'alan-mode-line)

(defcustom alan-mode-line/left ""
  "List of elements to include on the left of the mode-line."
  :type 'sexp
  :risky t
  :group 'alan-mode-line)
(defcustom alan-mode-line/center ""
  "List of elements to include in the center of the mode-line."
  :type 'sexp
  :risky t
  :group 'alan-mode-line)
(defcustom alan-mode-line/right ""
  "List of elements to include on the right of the mode-line."
  :type 'sexp
  :risky t
  :group 'alan-mode-line)
(defcustom alan-mode-line/minimal-other-windows t
  "Show a minimal mode-line on non-focused windows."
  :type 'boolean
  :group 'alan-mode-line)
(defcustom alan-mode-line/show-ace-wins-after 3
  "Include the Ace window number if this many windows are in frame."
  :type 'integer
  :group 'alan-mode-line)

(defun alan-mode-line/minify-p ()
  "Return t if mode-line elements should be minified on the current window."
  (and alan-mode-line/minimal-other-windows
       (not (mode-line-window-selected-p))))

(defun alan-mode-line/construct-mode-line (left center right)
  "Concatenate the LEFT, CENTER, and RIGHT elements for the mode line.
Use even spacing between them."
  (list
   left
   '(:eval (alan-mode-line/fill-center 'mode-line (reserve-left/middle)))
   center
   '(:eval (alan-mode-line/fill-right 'mode-line (reserve-middle/right)))
   right))

(setq-default mode-line-format (alan-mode-line/construct-mode-line
                                alan-mode-line/left
                                alan-mode-line/center
                                alan-mode-line/right))

(defun alan-mode-line/show-only-on-selected (construct)
  "Return CONSTRUCT for inclusion in the modeline when desired.
If window is active or alan-mode-line/minimal-other-windows is nil, show.
Otherwise return an empty string. Empty string seems to work better than nil."
  (if (not (alan-mode-line/minify-p))
      construct
    ""))

(defun alan-mode-line/fill-center (face reserve)
  "Fill the mode line with space characters in FACE from left to RESERVE.
RESERVE is a % of mode-line length."
  (unless reserve (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display
              `((space :align-to (- (+ center (.5 . right-margin))
                                    ,reserve
                                    (.5 . left-margin))))
              'face face))

(defconst alan-mode-line/RIGHT_PADDING 5
  "Amount of padding to add to right of mode-line.")

(defun alan-mode-line/fill-right (face reserve)
  "Fill the mode line with space characters in FACE from center to right.
RESERVE, as a % of mode-line length."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun reserve-left/middle ()
  "Calculate the amount of space to reserve from left to middle."
  ;; FIXME dividing by 2 should align center but 1.5 looks better
  (/ (length (format-mode-line alan-mode-line/center)) 1.5))
(defun reserve-middle/right ()
  "Calculate the amount of space to reserve from middle to right."
  (+ alan-mode-line/RIGHT_PADDING (length (format-mode-line alan-mode-line/right))))


;;; Faces:
(defface alan-mode-line/buffer-face
  `((t :inherit mode-line :weight normal
       :foreground ,(catppuccin-color 'rosewater)))
  "Buffer name in mode line when active and unmodified.")
(defface alan-mode-line/buffer-modified-face
  `((t :inherit mode-line :weight semibold :foreground ,(catppuccin-color 'maroon)))
  "Buffer name in mode line when modified.")
(defface alan-mode-line/buffer-ro-face
  `((t :inherit mode-line :weight normal :foreground ,(catppuccin-color 'blue)))
  "Buffer name in mode line when read-only.")
(defface alan-mode-line/buffer--unfocused-face
  `((t :inherit mode-line :foreground ,(catppuccin-color 'subtext0)))
  "Color for buffer title in mode-line when unfocused.")
(defface alan-mode-line/side-elements-face
  `((t :inherit mode-line :weight light :foreground ,(catppuccin-color 'subtext1)))
  "Side elements in mode line, which should blend in visually.")
(defface alan-mode-line/evil-state-normal-face
  `((t :inherit alan-mode-line/side-elements-face :weight bold :foreground ,(catppuccin-color 'green)))
  "Normal state mode line indicator.")
(defface alan-mode-line/evil-state-insert-face
  `((t :inherit alan-mode-line/evil-state-normal-face :foreground ,(catppuccin-color 'mauve)))
  "Insert state mode line indicator.")
(defface alan-mode-line/evil-state-visual-face
  `((t :inherit alan-mode-line/evil-state-normal-face :foreground ,(catppuccin-color 'flamingo)))
  "Visual state mode line indicator.")
;; TODO 2024-09 evil state faces for emacs state and possibly others.

;; TODO 2024-09 put this into proper control flow
(set-face-attribute 'mode-line nil :height 1.0 :inherit 'alan-mode-line/side-elements-face)

(set-face-attribute 'mode-line-active nil :overline (catppuccin-color 'subtext0)
                    :height 1.0)

;;; Functions for the mode-line elements:
;; Buffer name:
(defun alan-mode-line/format-title (buffer-str)
  "Set the face for buffer name BUFFER-STR.
Returns string with face attributes."
  (propertize
   buffer-str
   'face (cond ((buffer-modified-p)
                'alan-mode-line/buffer-modified-face)
               ((and (not (mode-line-window-selected-p))
                     alan-mode-line/minimal-other-windows)
                'alan-mode-line/buffer--unfocused-face)
               ((eval buffer-read-only)
                'alan-mode-line/buffer-ro-face)
               ((eval t)
                'alan-mode-line/buffer-face))))

(defun alan/modeline--buffer-name ()
  "Buffer name as formatted by alan-mode-line/format-title."
  (alan-mode-line/format-title (buffer-name)))
(defvar-local alan-mode-line/buffer-title
    '(:eval (alan/modeline--buffer-name))
  "Buffer title as formatted by helper functions.")

(defun alan-mode-line/ace-window-display ()
  "Show the window number if the window count is high enough and ace window exists.
Otherwise return the empty string."
  (if (and (>= (count-windows) alan-mode-line/show-ace-wins-after)
           ace-window-display-mode)
      (propertize (format " [window %s] "
                          (window-parameter
                           (selected-window)
                           'ace-window-path))
                  'face 'alan-mode-line/side-elements-face)
    ""))

(defun alan-mode-line/vc-mode-display ()
  "Show version control status, if any, in the mode line."
  (alan-mode-line/show-only-on-selected
   ;; test if vc-mode is active so we don't try to evaluate nil as a string:
   (if vc-mode
       (format "[%s]"
               (propertize
                (string-replace " " "" vc-mode)
                'face 'alan-mode-line/side-elements-face))
     "")))

(defun alan/modeline--major-mode ()
  "Helper function to get a proper string for the active major mode."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))
(defvar-local alan-mode-line/major-mode-name
    `(:eval (propertize (alan-mode-line/show-only-on-selected
                         (alan/modeline--major-mode))
                        'face 'alan-mode-line/side-elements-face))
  "The major mode name element for the mode-line.")

(defvar-local alan/modeline-mode-icon
    '(:eval (propertize (format "%s " (nerd-icons-icon-for-buffer))
                        'face 'alan-mode-line/side-elements-face))
  "Icon to be shown for the active major mode.")

(defvar-local alan-mode-line/evil-mode-line-tag
    `(:eval (propertize
             (alan-mode-line/show-only-on-selected
              evil-mode-line-tag)
             'face (cond
                    ((string= evil-state "normal")
                     'alan-mode-line/evil-state-normal-face)
                    ((string= evil-state "insert")
                     'alan-mode-line/evil-state-insert-face)
                    ((string= evil-state "visual")
                     'alan-mode-line/evil-state-visual-face)
                    ((eval t) 'alan-mode-line/side-elements-face))))
  "Formatted evil mode state tag for mode line.")

(defvar-local alan-mode-line/word-count
    `(:eval (alan-mode-line/show-only-on-selected
             (when text-mode-variant
               (propertize
                (format " %dW" (count-words (point-min) (point-max)))
                'face 'alan-mode-line/side-elements-face))))
  "Formatted word count for mode line, in `text-mode' variants like org.")

(defvar-local alan-mode-line/defining-kbd-macro
    `(:eval (alan-mode-line/show-only-on-selected
             (when defining-kbd-macro
               (propertize "MACRO" 'face
                           '(:foreground ,(catppuccin-color 'red)
                                         :weight bold)))))
  "Indicator in mode line when defining keyboard macro.")


(provide 'alternate-modeline)
;;; alternate-modeline.el ends here.
