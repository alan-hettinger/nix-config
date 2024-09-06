;;; keybinds.el  -*- lexical-binding: t -*-

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

  ;; prefix for all evil-window commands:
  "w" '(evil-window-map :which-key "window")

  ;; for all help commands:
  "h" '(help-command :which-key "help")

  "b" '(:ignore t :which-key "buffer")
  "bk" '(kill-current-buffer :which-key "close")
  "bb" '(ibuffer :which-key "ibuffer")

  "t" '(:ignore t :which-key "toggle")
  "tl" '(display-line-numbers-mode :which-key "line numbers")
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
  "gt" '(magit-todos-list :which-key "list todos"))

(when (functionp 'helpful-callable)
  (general-define-key
   [remap describe-function] #'helpful-callable
   [remap describe-key] #'helpful-key
   [remap describe-variable] #'helpful-variable
   [remap view-hello-file] #'helpful-at-point ;; [leader]-h-h. view-hello-file is kind of pointless
   ))
