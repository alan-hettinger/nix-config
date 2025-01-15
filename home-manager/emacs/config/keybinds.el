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


(use-package general
  :defer nil
  :demand t
  :config
  (general-create-definer alan/leader-keys
    :states 'normal
    :keymaps 'override
    :prefix "SPC")

;;; Functions used by keybinds:
  (defun alan-binds/switch-buffer-maybe-project ()
    "Switch buffers using preferred command based on context"
    (interactive)
    (cond ((and projectile-project-name
                (fboundp 'consult-project-buffer))
           (consult-project-buffer))
          ((fboundp 'consult-buffer)
           (consult-buffer))
          ((eval t) (call-interactively 'switch-to-buffer))))

  (defun alan-binds/switch-buffer-maybe-persp ()
    "Switch buffer, within perspective if available."
    (interactive)
    (let ((switch-command 'persp-switch-to-buffer*))
      (cond ((fboundp switch-command) (call-interactively switch-command))
            ((fboundp 'consult-buffer) (consult-buffer))
            ((eval t) (call-interactively 'switch-to-buffer)))))

  (defun alan-binds/ibuffer-maybe-persp ()
    "Open ibuffer, with persp-ibuffer when available"
    (interactive)
    (if persp-mode
        (persp-ibuffer nil)
      (ibuffer)))
  (defun alan-binds/revert-buffer-noconfirm ()
    "Revert buffer without confirming"
    (interactive)
    (revert-buffer nil t))

  (defun alan-binds/quit-dwim ()
    "Complete server-edit, or delete frame, or kill emacs."
    (interactive)
    (cond (server-buffer-clients (server-edit))
          ((< 2 (length (visible-frame-list))) (delete-frame))
          (t (save-buffers-kill-emacs))))

;;; Leader key binds:
  (alan/leader-keys
    "SPC" '(alan-binds/switch-buffer-maybe-persp :which-key "switch buffers")
    "." '(find-file :which-key "find file")
    "/" '(consult-line :which-key "find-line")

    "s" '(consult-ripgrep :which-key "search directory or project")

    ;; prefix for all evil-window commands:
    "w" '(evil-window-map :which-key "window")
    "wm" '(delete-other-windows :which-key "maximize")

    "W" #'window-toggle-side-windows

    ;; for all help commands:
    "h" '(help-command :which-key "help")

    "t" '(:ignore t :which-key "toggle")
    "tl" '(display-line-numbers-mode :which-key "line numbers")
    "tm" '(alan/minimal-ui-mode :which-key "minimal UI")
    "tp" '(alan/padding-mode :which-key "padding")

    "o" '(:ignore t :which-key "open")
    "op" '(treemacs :which-key "treemacs")

    "f" '(:ignore t :which-key "file")
    "fs" '(save-buffer :which-key "save")
    "ff" '(find-file :which-key "find")
    "fr" '(recentf :which-key "recent"))


  ;; wrappers for specific key maps:
  (general-create-definer alan/quit-map-definer
    :wrapping alan/leader-keys
    :infix "q")
  (alan/quit-map-definer
    "" '(:ignore t :which-key "quit")
    "q" 'alan-binds/quit-dwim
    "e" 'server-edit
    "E" 'server-edit-abort
    "f" 'delete-frame
    "b" 'kill-current-buffer
    "w" 'delete-window)

  (general-create-definer alan/buffer-map-definer
    :wrapping alan/leader-keys
    :infix "b")
  (alan/buffer-map-definer
    "" '(nil :which-key "buffer")
    "k" '(kill-current-buffer :which-key "close")
    ;; "b" '(alan-binds/ibuffer-maybe-persp :which-key "ibuffer")
    "b" 'ibuffer
    "r" '(alan-binds/revert-buffer-noconfirm :which-key "revert")
    "c" '(clone-indirect-buffer :which-key "clone"))

;;; Definers to be used by packages:
  (general-create-definer alan/local-leader
    :wrapping alan/leader-keys
    :infix "m")
  (alan/local-leader
    "" '(:ignore t :which-key "local mode"))

  (general-create-definer alan/lsp-map-definer
    :wrapping alan/leader-keys
    :infix "l")
  (alan/lsp-map-definer
    "" '(:ignore t :which-key "LSP"))

  (general-create-definer alan/vc-map-definer
    :wrapping alan/leader-keys
    :infix "g")
  (alan/vc-map-definer "" '(:ignore t :which-key "git"))

  (general-create-definer alan/org-global-map-definer
    ;; Only for globally-accessible org bindings.
    :wrapping alan/leader-keys
    :infix "n")
  (alan/org-global-map-definer "" '(:ignore t :which-key "org"))

  (general-define-key :states 'normal
                      "C-=" #'text-scale-increase
                      "C--" #'text-scale-decrease))


(provide 'keybinds)
