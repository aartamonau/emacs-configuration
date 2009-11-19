(load "~/emacs/extensions-management.el")

;; custom custom-file :)
(setq custom-file "~/emacs/custom.el")
(load custom-file 'noerror)

;; no disabled commands for novice (does not clobber .emacs file)
(setq disabled-command-function nil)

;; rc must be loaded after custom-file
(load "~/emacs/rc.el")
