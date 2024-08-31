(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(general-create-definer alan/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(alan/leader-keys
  "SPC" '(consult-buffer :which-key "switch buffers")
  "," '(consult-buffer :which-key "switch buffers")
  "." '(find-file :which-key "find file")
  "/" '(consult-line :which-key "find-line")
  
  "t" '(:ignore t :which-key "toggle")
  "tl" '(display-line-numbers-mode :which-key "line numbers")
  "o" '(:ignore t :which-key "open")
  "op" '(treemacs :which-key "treemacs")
  "oe" '(eshell :which-key "eshell")

  "p" '(:ignore t :which-key "project")
  "pp" '(projectile-switch-project "switch project")

  "f" '(:ignore t :which-key "file")
  "fs" '(save-buffer :which-key "save")
  "ff" '(find-file :which-key "find")
  "fr" '(recentf :which-key "recent")

  "g" '(:ignore t :which-key "magit")
  "gg" '(magit :which-key "open magit")
  "gs" '(magit-stage-buffer-file :which-key "stage current file")
  "gS" '(magit-stage-file :which-key "stage file")

  "d" '(:ignore t :which-key "dired")
  "dd" '(dired-jump :which-key "open dired here"))
