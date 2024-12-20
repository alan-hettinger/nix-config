;;; package --- completion-config.el  -*- lexical-binding: t -*-

;;; Commentary:
;; TODO

;;; Code:

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  (corfu-scroll-margin 5)
  (corfu-preselect 'directory)
  (corfu-popupinfo-delay 0.5)
  :config
  (corfu-popupinfo-mode)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode)))
  :general
  (:keymaps 'corfu-map
            ;; Only complete on TAB unless in shell modes:
            "RET" `(menu-item "" nil :filter
                              ,(lambda (&optional _)
                                 (and (derived-mode-p
                                       'eshell-mode 'comint-mode)
                                      #'corfu-send)))
            "M-SPC" 'corfu-insert-separator))

;; Added completion functionality with `corfu'
(use-package cape
  :after corfu
  :defer nil
  :demand t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package which-key
  :hook after-init
  :custom
  (which-key-popup-type 'minibuffer))

(add-hook 'after-init-hook #'electric-pair-mode) ;; TODO wrong file?

(use-package vertico
  :hook after-init
  :init
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'vertico-mode-hook 'marginalia-mode)
  :custom
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (vertico-resize t)
  (vertico-count 15)
  (vertico-cycle t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (vertico-reverse-mode 1)
  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous
            ;; NOTE: not binding "C-l" to 'vertico-directory-enter',
            ;; does not always DWIM.
            "C-h" 'vertico-directory-up)
  (:keymaps 'vertico-reverse-map
            ;; "up" and "down" have opposite meanings:
            "C-j" 'vertico-previous
            "C-k" 'vertico-next))

(use-package savehist
  :hook after-init
  :custom
  (savehist-file (expand-file-name "history" alan/cache-dir)))

(use-package orderless
  :after vertico
  :defer nil
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :custom
  (consult-async-min-input 3)
  (consult-async-refresh-delay 0.2)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  :general
  ([remap bookmark-jump] #'consult-bookmark
   [remap evil-show-registers] #'consult-register
   [remap goto-line] #'consult-goto-line
   [remap imenu] #'consult-imenu
   [remap Info-search] #'consult-info
   [remap locate] #'consult-locate
   [remap man] #'consult-man
   [remap recentf] #'consult-recent-file
   [remap yank-pop] #'consult-yank-pop
   [remap switch-to-buffer] #'consult-buffer)
  (alan/local-leader :keymaps 'prog-mode-map
    "c" #'consult-flycheck)
  )

(use-package marginalia)

(provide 'completion-config)
