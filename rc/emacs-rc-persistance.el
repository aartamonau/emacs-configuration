;; Save state of emacs on exit.

;; Reopen files
(desktop-save-mode 1)
(custom-set-variables
 '(desktop-restore-eager 10))

;; Save environment
;; (require 'session)
;; (add-hook 'after-init-hook 'session-initialize)

;; Save point in buffer
(setq-default save-place t)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
