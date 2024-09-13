;;; keybinds.el  -*- lexical-binding: t -*-

(define-minor-mode alan/minimal-ui-mode
  "Hide as many extraneous UI elements as possible."
  :init-value nil
  :interactive t
  :global t
  :lighter "Min"
  (if alan/minimal-ui-mode
      (progn (when (functionp 'hide-mode-line-mode)
               (global-hide-mode-line-mode 1))
             (when (window-with-parameter 'window-side nil)
               (window-toggle-side-windows))
             (delete-other-windows)
             (diff-hl-mode -1))
    (progn (when (functionp 'hide-mode-line-mode)
             (global-hide-mode-line-mode -1))
           (window-toggle-side-windows)
           (diff-hl-mode 1))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(general-create-definer alan/leader-keys
  :states 'normal
  :keymaps 'override
  :prefix "SPC")

(general-create-definer alan/local-leader
  :states 'normal
  :prefix "SPC m")

(alan/leader-keys
  "SPC" '(consult-buffer :which-key "switch buffers")
  "." '(find-file :which-key "find file")
  "/" '(consult-line :which-key "find-line")

  "s" '(consult-ripgrep :which-key "search directory or project")

  ;; prefix for all evil-window commands:
  "w" '(evil-window-map :which-key "window")
  "wm" '(delete-other-windows :which-key "maximize")

  ;; for all help commands:
  "h" '(help-command :which-key "help")

  "b" '(:ignore t :which-key "buffer")
  "bk" '(kill-current-buffer :which-key "close")
  "bb" '(ibuffer :which-key "ibuffer")

  "t" '(:ignore t :which-key "toggle")
  "tl" '(display-line-numbers-mode :which-key "line numbers")
  "tm" '(alan/minimal-ui-mode :which-key "minimal UI")

  "o" '(:ignore t :which-key "open")
  "op" '(treemacs :which-key "treemacs")
  "oe" '(eshell :which-key "eshell")

  "dd" '(dired-jump :which-key "dired here")

  ;; all projectile command keys:
  "p" '(projectile-command-map :which-key "project")

  "f" '(:ignore t :which-key "file")
  "fs" '(save-buffer :which-key "save")
  "ff" '(find-file :which-key "find")
  "fr" '(recentf :which-key "recent")

  "g" '(:ignore t :which-key "magit")
  "gg" '(magit-status :which-key "open magit")
  "gs" '(magit-stage-buffer-file :which-key "stage current file")
  "gS" '(magit-stage-file :which-key "stage file")
  "gt" '(magit-todos-list :which-key "list todos")
  "gc" '(magit-commit :which-key "commit"))

(when (functionp 'helpful-callable)
  (general-define-key
   [remap describe-function] #'helpful-callable
   [remap describe-key] #'helpful-key
   [remap describe-variable] #'helpful-variable
   [remap view-hello-file] #'helpful-at-point ;; [leader]-h-h. view-hello-file is kind of pointless
   ))

(provide 'keybinds)
