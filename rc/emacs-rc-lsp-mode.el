(custom-set-variables
 '(lsp-keymap-prefix "C-.")
 '(lsp-restart 'ignore))

(require 'lsp-mode)

(add-hook 'lsp-mode-hook
          (lambda nil
            (setq-local company-idle-delay nil)
            (local-set-key (kbd "M-TAB") 'company-complete)))