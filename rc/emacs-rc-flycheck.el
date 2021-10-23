(custom-set-variables
 '(flycheck-standard-error-navigation nil))

(global-set-key (kbd "C-c ?") 'flycheck-list-errors)
(global-set-key (kbd "C-c n") 'flycheck-next-error)
(global-set-key (kbd "C-c p") 'flycheck-previous-error)
