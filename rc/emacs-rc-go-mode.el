(add-hook 'go-mode-hook
          (lambda () (c-subword-mode)))
(add-hook 'before-save-hook 'gofmt-before-save)
