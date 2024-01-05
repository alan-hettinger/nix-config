;;; $DOOMDIR/appearance.el -*- lexical-binding: t; -*-

;; font settings:
(setq doom-font (font-spec :family "mononoki" :size 16.0))
(setq doom-variable-pitch-font (font-spec :family "Source Serif Variable" :height 1.2 :size 16.0))

(use-package! mixed-pitch
  :config
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 1.2)
  )

;; theme settings:
(use-package! catppuccin-theme
  :config (load-theme 'catppuccin t)
  :init (setq catppuccin-flavor 'macchiato))
(setq doom-theme 'catppuccin)

;; line numbers:
(setq display-line-numbers-type 'relative)
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                eshell-mode-hook
                markdown-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; disable title bar in gnome
(setq default-frame-alist '((undecorated . t)))
(setq lsp-headerline-breadcrumb-enable t)

;; remove " - Doom Emacs" from the window name
(setq frame-title-format '("%b"))

;; modeline:
(after! doom-modeline
  (setq doom-modeline-enable-word-count t
        doom-modeline-major-mode-icon t
        doom-modeline-persp-name t
        doom-modeline-height 35
        doom-modeline-icon t
        doom-modeline-modal-modern-icon nil
        doom-modeline-buffer-modification-icon t
        doom-modeline-highlight-modified-buffer-name nil
        )
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (remove-hook 'doom-modeline-mode-hook #'column-number-mode)
  (line-number-mode -1))

;; the doom dashboard:
(defun doom-custom-banner ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'` ' '`---^`---'`---'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32))) "\n"))
     'face 'doom-dashboard-banner)))
(setq +doom-dashboard-ascii-banner-fn #'doom-custom-banner)

;; for light and dark themes:
(defun toggle-catppuccin-light-dark-theme ()
  "toggle between light and dark catppuccin colorschemes"
  (interactive (progn
                 (let ((active-theme catppuccin-flavor)
                       (light-theme 'latte)
                       (dark-theme 'macchiato))
                   ((lambda (desired-theme) (and (setq catppuccin-flavor desired-theme) (catppuccin-reload)))
                    (if (eq active-theme dark-theme) light-theme dark-theme)))
                 nil)))

(defun toggle-light-dark-theme ()
  "toggle between light and dark theme generally"
  (interactive (progn
                 (let* ((active-theme doom-theme)
                        (light-theme 'doom-gruvbox-light)
                        (dark-theme 'catppuccin))
                   (load-theme (if (eq doom-theme dark-theme) light-theme dark-theme)))
                 nil)))

(map! :leader
      (:prefix "t"
       :desc "toggle light/dark mode" "d" #'toggle-light-dark-theme))

