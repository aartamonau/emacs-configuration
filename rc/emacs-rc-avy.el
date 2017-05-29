(require 'avy)
(require 'ace-window)

(setq aw-dispatch-always nil)

(global-set-key (kbd "C-c j") 'avy-goto-subword-0)
(global-set-key (kbd "C-c l") 'avy-goto-line)
(global-set-key (kbd "C-x o") 'ace-window)

(ace-link-setup-default)
(global-set-key (kbd "M-o") 'ace-link-addr)
