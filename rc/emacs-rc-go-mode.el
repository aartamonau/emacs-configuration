(add-hook 'go-mode-hook
          (lambda () (c-subword-mode)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'my/godef-jump)))

(defun my/godef-jump (point other-window)
  (interactive "d\nP")
  (godef-jump point other-window))
