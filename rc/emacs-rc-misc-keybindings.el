(defun my/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'my/kill-this-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; set to default
(global-set-key (kbd "C-=") (lambda ()
                              (interactive)
                              (text-scale-set 0)))

(global-set-key (kbd "C-h M") 'man)
