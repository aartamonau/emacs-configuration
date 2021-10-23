(require 'go-mode)
(require 'go-guru)

(add-hook 'go-mode-hook
          (lambda () (c-subword-mode)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'my/godef-jump)))
(add-hook 'go-mode-hook 'my/dont-highlight-tabs)

(defun my/godef-jump (point other-window)
  (interactive "d\nP")
  (godef-jump point other-window))

(defun my/dont-highlight-tabs ()
  (make-variable-buffer-local 'whitespace-style)
  (setq whitespace-style (delq 'tabs whitespace-style)))

(custom-set-variables
 '(godoc-use-completing-read nil)
 '(godoc-command "go doc -all"))
