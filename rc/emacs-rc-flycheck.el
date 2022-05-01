(custom-set-variables
 '(flycheck-standard-error-navigation nil))

;; make errors more visible
(custom-set-faces
 '(flycheck-error ((t (:background "#500000" :underline nil))))
 '(flycheck-info ((t (:background "#005000" :underline nil))))
 '(flycheck-warning ((t (:background "#d04000" :underline nil)))))


(global-set-key (kbd "C-c ?") 'flycheck-list-errors)
(global-set-key (kbd "C-c n") 'flycheck-next-error)
(global-set-key (kbd "C-c p") 'flycheck-previous-error)
