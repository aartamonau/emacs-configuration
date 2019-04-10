(require 'avy)
(require 'ace-window)

(setq aw-dispatch-always nil)

(setq avy-style 'de-bruijn)
(setq avy-background t)
(setq avy-timeout-seconds 0.3)
(setq avy-all-windows nil)
(setq avy-all-windows-alt t)
(setq avy-subword-extra-word-chars nil)
(setq aw-scope 'frame)

(custom-set-faces
 '(avy-goto-char-timer-face ((t (:inherit isearch)))))

(global-set-key (kbd "C-c j") 'avy-goto-subword-1)
(global-set-key (kbd "C-c l") 'avy-goto-line)
(global-set-key (kbd "C-c C-j") 'avy-goto-subword-1)
(global-set-key (kbd "C-c C-l") 'avy-goto-line)
(global-set-key (kbd "C-c C-w") 'avy-kill-region)
(global-set-key (kbd "C-c M-w") 'avy-kill-ring-save-region)
(global-set-key (kbd "C-c k") 'avy-goto-char-timer)
(global-set-key (kbd "C-c C-k") 'avy-goto-char-timer)
(global-set-key (kbd "C-c m") 'avy-move-region)
(global-set-key (kbd "C-c M") 'avy-move-line)
(global-set-key (kbd "C-x o") 'ace-window)

(ace-link-setup-default)
(global-set-key (kbd "M-o") 'ace-link-addr)
