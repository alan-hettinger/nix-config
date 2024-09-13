;; alternate-modeline.el -*- lexical-binding: t -*-

(defgroup alan-mode-line nil
  "Custom mode line."
  :group 'mode-line)
(defgroup alan-mode-line/faces nil
  "Faces for my custom mode line."
  :group 'alan-mode-line)

(defcustom alan/mode-line-left ""
  "List of elements to include on the left of the mode-line."
  :type 'sexp
  :risky t
  :group 'alan-mode-line)
(defcustom alan/mode-line-center ""
  "List of elements to include in the center of the mode-line."
  :type 'sexp
  :risky t
  :group 'alan-mode-line)
(defcustom alan/mode-line-right ""
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

(defun alan-mode-line/--minify-p ()
  "Returns t if mode-line elements should be minified on the current window."
  (and alan-mode-line/minimal-other-windows
       (not (mode-line-window-selected-p))))

(defun alan-mode-line/--construct-mode-line (left center right)
  (list
   left
   '(:eval (alan/mode-line-fill-center 'mode-line (reserve-left/middle)))
   center
   '(:eval (alan/mode-line-fill-right 'mode-line (reserve-middle/right)))
   right))

(setq-default mode-line-format (alan-mode-line/--construct-mode-line
                                alan/mode-line-left
                                alan/mode-line-center
                                alan/mode-line-right))

(defun alan/mode-line--show-only-on-selected (construct)
  "Return CONSTRUCT for inclusion in the modeline if the window is active
or alan-mode-line/minimal-other-windows is nil.
Otherwise return an empty string. Empty string seems to work better than nil."
  (if (not (alan-mode-line/--minify-p))
      construct
    ""))

(defun alan/mode-line-fill-center (face reserve)
  "Fill the mode line with space characters in FACE from left to RESERVE, as a % of mode-line length."
  (unless reserve (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display
              `((space :align-to (- (+ center (.5 . right-margin))
                                    ,reserve
                                    (.5 . left-margin))))
              'face face))

(defconst RIGHT_PADDING 5
  "Amount of padding to add to right of mode-line.")

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


;;; Faces:
(defface alan/mode-line--buffer-face
  `((t :inherit mode-line :weight normal :foreground ,(catppuccin-color 'rosewater)))
  "test docstring")
(defface alan/mode-line--buffer-modified-face
  `((t :inherit mode-line :weight semibold :foreground ,(catppuccin-color 'maroon)))
  "test docstring")
(defface alan/mode-line--buffer-ro-face
  `((t :inherit mode-line :weight normal :foreground ,(catppuccin-color 'blue)))
  "fake docstring")
(defface alan-mode-line/buffer--unfocused-face
  `((t :inherit mode-line :foreground ,(catppuccin-color 'subtext2)))
  "Color for buffer title in mode-line when unfocused.")
(defface alan-mode-line/side-elements-face
  `((t :inherit mode-line :weight light :foreground ,(catppuccin-color 'subtext1)))
  "placeholder docstring")

(defface alan-mode-line/evil-state-normal-face
  `((t :inherit alan-mode-line/side-elements-face :weight bold :foreground ,(catppuccin-color 'green)))
  "placeholder docstring")
(defface alan-mode-line/evil-state-insert-face
  `((t :inherit alan-mode-line/evil-state-normal-face :foreground ,(catppuccin-color 'mauve)))
  "placeholder-docstring")
(defface alan-mode-line/evil-state-visual-face
  `((t :inherit alan-mode-line/evil-state-normal-face :foreground ,(catppuccin-color 'flamingo)))
  "placeholder docstring")

(set-face-attribute 'mode-line nil :height 1.0 :inherit 'alan-mode-line/side-elements-face)

(set-face-attribute 'mode-line-active nil :overline (catppuccin-color 'subtext0)
                    :height 1.0)

;;; Functions for the mode-line elements:
;; Buffer name:
(defun alan/mode-line--format-title (buffer-str)
  (propertize
   buffer-str
   'face (cond ((buffer-modified-p)
                'alan/mode-line--buffer-modified-face)
               ((and (not (mode-line-window-selected-p))
                     alan-mode-line/minimal-other-windows)
                'alan-mode-line/buffer--unfocused-face)
               ((eval buffer-read-only)
                'alan/mode-line--buffer-ro-face)
               ((eval t)
                'alan/mode-line--buffer-face))))

(defun alan/modeline--buffer-name ()
  (alan/mode-line--format-title (buffer-name)))
(defvar-local alan/mode-line-buffer-title
    '(:eval (alan/modeline--buffer-name)))

(defun alan-mode-line/ace-window-display ()
  "Show the window number if the window count is high enough and ace window
exists. Otherwise return the empty string."
  (if (and (>= (count-windows) alan-mode-line/show-ace-wins-after)
           ace-window-display-mode)
      (propertize (format " [window %s] "
                          (window-parameter
                           (selected-window)
                           'ace-window-path))
                  'face 'alan-mode-line/side-elements-face)
    ""))

(defun alan-mode-line/vc-mode-display ()
  (alan/mode-line--show-only-on-selected
   (format "[%s]"
           (propertize
            (string-replace " " "" vc-mode)
            'face 'alan-mode-line/side-elements-face))))

;; Major mode:
(defun alan/modeline--major-mode ()
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))
(defvar-local alan/modeline-mode-icon
    '(:eval (propertize (format "%s " (nerd-icons-icon-for-buffer))
                        'face 'alan-mode-line/side-elements-face)))
(defvar-local alan/mode-line-major-mode-name
    `(:eval (propertize (alan/mode-line--show-only-on-selected
                         (alan/modeline--major-mode))
                        'face 'alan-mode-line/side-elements-face)))
(defvar-local alan/mode-line--evil-mode-line-tag
    `(:eval (propertize
             (alan/mode-line--show-only-on-selected
              evil-mode-line-tag)
             'face (cond
                    ((string= evil-state "normal")
                     'alan-mode-line/evil-state-normal-face)
                    ((string= evil-state "insert")
                     'alan-mode-line/evil-state-insert-face)
                    ((string= evil-state "visual")
                     'alan-mode-line/evil-state-visual-face)
                    ((eval t) 'alan-mode-line/side-elements-face)))))
(defvar-local alan-mode-line/word-count
    `(:eval (alan/mode-line--show-only-on-selected
             (when text-mode-variant
               (propertize
                (format " %dW" (count-words (point-min) (point-max)))
                'face 'alan-mode-line/side-elements-face)))))
(defvar-local alan-mode-line/defining-kbd-macro
    `(:eval (alan/mode-line--show-only-on-selected
             (when defining-kbd-macro
               (propertize "MACRO" 'face
                           '(:foreground ,(catppuccin-color 'red)
                                         :weight bold))))))

(setq alan/mode-line-left
      '("%e"
        (:eval alan/mode-line--evil-mode-line-tag)
        (:eval (alan-mode-line/ace-window-display))
        (:eval (alan/mode-line--show-only-on-selected mode-line-position))
        (:eval alan-mode-line/word-count)
        (:eval alan-mode-line/defining-kbd-macro)))
(setq alan/mode-line-center
      '((:eval alan/modeline-mode-icon)
        (:eval alan/mode-line-buffer-title)))
(setq alan/mode-line-right
      '(
        (:eval (anzu--update-mode-line))
        (:eval (alan/mode-line--show-only-on-selected mode-line-misc-info))
	    (:eval (alan-mode-line/vc-mode-display))
        "  "
        (:eval alan/mode-line-major-mode-name)))
