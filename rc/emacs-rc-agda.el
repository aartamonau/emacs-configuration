(when (executable-find "agda-mode")
  (setq agda2-include-dirs `("." "~/Agda/lib/" "/usr/lib/agda"))
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))
