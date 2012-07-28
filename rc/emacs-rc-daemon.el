(when (daemonp)
  (defun my/daemon-maybe-save-buffers-kill-emacs()
    "Kill emacs server with the last client. Otherwise just close the client"
    (interactive)
    ;; when only one client left just kill the server
    (if (= (length server-clients) 0)
        (save-buffers-kill-emacs)
      ;; otherwise just kill current client
      (save-buffers-kill-terminal)))

  (define-key global-map (kbd "C-x C-c")
    'my/daemon-maybe-save-buffers-kill-emacs))
