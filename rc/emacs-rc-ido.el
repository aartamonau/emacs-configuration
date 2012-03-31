(ido-mode t)

(require 'ido-ubiquitous)
(ido-ubiquitous t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(custom-set-variables
 '(ido-show-dot-for-dired t)
 '(ido-enable-flex-matching t))
