
(add-hook 'company-mode-hook
	  (lambda () (setq company-minimum-prefix-length 3
			   company-idle-delay 1.0
			   company-tooltip-align-annotations t
			   company-tooltip-flip-when-above t
			   company-show-quick-access 'left)))
(global-company-mode)
(which-key-mode)
(electric-pair-mode)

(vertico-mode 1)
(savehist-mode 1)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles partial-completion))))
(require 'marginalia)
(marginalia-mode 1)
