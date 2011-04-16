(custom-set-variables
 '(reb-re-syntax 'string))

(add-hook 'reb-mode-hook
          '(lambda ()
             (define-key reb-mode-map (kbd "C-s") 'reb-next-match)
             (define-key reb-mode-map (kbd "C-r") 'reb-prev-match)))

(global-set-key (kbd "C-c C-r") 're-builder)
