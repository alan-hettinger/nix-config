;;; package --- completion-config.el  -*- lexical-binding: t -*-

;;; Commentary:
;; TODO

;;; Code:

(add-hook 'company-mode-hook
	      (lambda () (setq company-minimum-prefix-length 3
			               company-idle-delay 0.2
			               company-tooltip-align-annotations t
			               company-tooltip-flip-when-above t
			               company-show-quick-access 'left
                           company-dabbrev-ignore-case 'keep-prefix)))

;; for org, markdown, etc:
(defun alan/text-mode-company-hook ()
  (setq-local company-backends
              '((company-dabbrev company-ispell :separate)
                company-files)))
(add-hook 'text-mode-hook #'alan/text-mode-company-hook)

(global-company-mode)
(add-hook 'which-key-mode-hook
          (lambda () (setq which-key-popup-type 'minibuffer)))
(which-key-mode)
(electric-pair-mode)

(vertico-mode 1)
(savehist-mode 1)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt)
      vertico-resize nil
      vertico-count 15
      vertico-cycle t)
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles partial-completion))))
(setq consult-async-min-input 3
      consult-async-refresh-delay 0.2
      consult-async-input-throttle 0.2
      consult-async-input-debounce 0.1)
(require 'marginalia)
(marginalia-mode 1)

(general-define-key :keymaps 'vertico-map
                    "C-j" 'vertico-next)
(general-define-key :keymaps 'vertico-map
                    "C-k" 'vertico-previous)
(general-define-key
 [remap bookmark-jump] #'consult-bookmark
 [remap evil-show-registers] #'consult-register
 [remap goto-line] #'consult-goto-line
 [remap imenu] #'consult-imenu
 [remap Info-search] #'consult-info
 [remap locate] #'consult-locate
 [remap man] #'consult-man
 [remap recentf-open] #'consult-recent-file
 [remap yank-pop] #'consult-yank-pop)

(keymap-global-set "C-x b" #'consult-buffer)
