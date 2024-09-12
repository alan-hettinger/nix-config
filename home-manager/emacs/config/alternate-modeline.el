
(kill-local-variable 'mode-line-format)
(force-mode-line-update)

(defun alan/mode-line--show-only-on-selected (construct)
  (when (mode-line-window-selected-p) construct))

(defface alan/mode-line--buffer-face
  `((t :inherit bold :foreground ,(catppuccin-color 'blue)))
  "test docstring")

(defun alan/modeline--buffer-name ()
  (propertize (format "  %s  " ( buffer-name)) 'face 'alan/mode-line--buffer-face))

(defun alan/modeline--major-mode ()
  (capitalize (symbol-name major-mode)))

(defvar-local alan/mode-line-buffer-title
    '(:eval (alan/modeline--buffer-name)))

(defvar-local alan/mode-line-major-mode-name
    '(:eval (alan/mode-line--show-only-on-selected (alan/modeline--major-mode))))

(put 'alan/mode-line-buffer-title 'risky-local-variable t)
(put 'alan/mode-line-major-mode-name 'risky-local-variable t)


(setq-default mode-line-format
              '("%e"
                (:eval evil-mode-line-tag)
                alan/mode-line-buffer-title
		        mode-line-position
				alan/mode-line-major-mode-name
                ))
