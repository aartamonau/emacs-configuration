;; Save state of emacs on exit.

;; Reopen files
(desktop-save-mode 1)
(custom-set-variables
 '(desktop-restore-eager 10))

(desktop-auto-save-enable 10)

(defun desktop-force-read ()
  (interactive)
  (let ((desktop-load-locked-desktop t))
    (desktop-read)))

;; be conservative about loaded locked desktop files when running daemon
(when (daemonp)
  (setq desktop-load-locked-desktop nil))

;; Save environment
;; (require 'session)
;; (add-hook 'after-init-hook 'session-initialize)

;; Save point in buffer
(setq-default save-place t)

(savehist-mode 1)
(setq savehist-additional-variables '(search-ring regexp-search-ring))
