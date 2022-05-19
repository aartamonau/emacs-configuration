(require 'ace-window)

(setq aw-dispatch-always nil)
(setq aw-scope 'frame)
(global-set-key (kbd "C-x o") 'ace-window)

(ace-link-setup-default)
(global-set-key (kbd "M-o") 'ace-link-addr)
