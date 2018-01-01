(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; set to default
(global-set-key (kbd "C-=") (lambda ()
                              (interactive)
                              (text-scale-set 0)))
