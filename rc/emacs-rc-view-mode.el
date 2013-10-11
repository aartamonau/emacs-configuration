(defun my/maybe-view-mode (&rest args)
  (let ((major-mode-name (symbol-name major-mode)))
    (unless (or (string-prefix-p "magit-" major-mode-name)
                (string-equal 'rebase-mode major-mode)
                (string-equal 'git-rebase-mode major-mode))
      (apply 'view-mode args))))

(defadvice toggle-read-only (after run-view-mode-on-read-only activate)
  "Activates view-mode in buffer if it is made read-only"
  (let ((toggle (if buffer-read-only 1 0)))
    (my/maybe-view-mode toggle)))

(defun my/find-file-hook ()
  "Activates view-mode if buffer is read-only"
  (when buffer-read-only
    (my/maybe-view-mode)))

(add-hook 'find-file-hook 'my/find-file-hook)

(defun my/view-mode-hook ()
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "l") 'forward-char)

  (define-key view-mode-map (kbd "G")
    (lambda ()
      (interactive)
      (View-goto-percent 100)))

  (define-key view-mode-map (kbd "q") 'delete-window)
  (define-key view-mode-map (kbd "Q") 'kill-buffer-and-window))

(add-hook 'view-mode-hook 'my/view-mode-hook)
