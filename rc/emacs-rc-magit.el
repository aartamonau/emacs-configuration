(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(define-key magit-log-mode-map (kbd "j") 'magit-goto-next-section)
(define-key magit-log-mode-map (kbd "k") 'magit-goto-previous-section)

;; ignore whitespace in git blame
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

(defun magit-commit-amend ()
  (interactive)
  (magit-commit t))

(define-key magit-mode-map (kbd "C-c C-c") 'magit-commit)
(define-key magit-mode-map (kbd "C-c C-a") 'magit-commit-amend)

;; magit-view-file-history needs this
(setq magit-sha1-abbrev-length (magit-abbrev-length))

(defadvice magit-view-file-at-commit (around
                                      magit-view-file-at-commit-reuse-buffer
                                      activate)
  (flet ((generate-new-buffer (name)
                              (if (get-buffer name)
                                  (kill-buffer name))
                              (get-buffer-create name)))
    ad-do-it))
