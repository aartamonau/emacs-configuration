(require 'go-mode)

(add-hook 'go-mode-hook
          (lambda () (c-subword-mode)))
(add-hook 'go-mode-hook 'my/dont-highlight-tabs)
(add-hook 'go-mode-hook 'lsp)

(defun my/dont-highlight-tabs ()
  (make-variable-buffer-local 'whitespace-style)
  (setq whitespace-style (delq 'tabs whitespace-style)))

(custom-set-variables
 '(godoc-use-completing-read nil)
 '(godoc-command "go doc -all"))
