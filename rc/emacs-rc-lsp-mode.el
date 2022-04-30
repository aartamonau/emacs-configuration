(custom-set-variables
 '(lsp-keymap-prefix "C-.")
 '(lsp-restart 'ignore)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-modeline-diagnostics-enable nil)
 '(lsp-modeline-code-actions-enable nil))

(require 'lsp-mode)

(add-hook 'lsp-mode-hook
          (lambda nil
            (setq-local company-idle-delay nil)
            (local-set-key (kbd "M-TAB") 'company-complete)))
