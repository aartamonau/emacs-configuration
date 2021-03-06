(toggle-diredp-find-file-reuse-dir 1)

(custom-set-variables
 '(dired-recursive-deletes 'always)
 '(diredp-hide-details-initially-flag nil))

(defun my/open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app."
  (interactive)
  (let (do-it
        (file-list
         (cond
          ((string-equal major-mode "dired-mode") (dired-get-marked-files))
          ((not file) (list (buffer-file-name)))
          (file (list file)))))

    (setq do-it (if (<= (length file-list) 5)
                    t
                  (y-or-n-p "Open more than 5 files? ")))
    (when do-it
      (mapc (lambda (path)
              (let ((process-connection-type nil))
                (start-process "" nil "xdg-open" path)))
            file-list))))

(define-key dired-mode-map (kbd "<C-return>") 'my/open-in-external-app)

(define-key dired-mode-map (kbd "M-g") nil)
(define-key dired-mode-map (kbd "M-g n") 'next-error)
(define-key dired-mode-map (kbd "M-g p") 'previous-error)
(define-key dired-mode-map (kbd "M-G") 'diredp-do-grep)

(require 'dired-ranger)
(define-key dired-mode-map (kbd "W") 'dired-ranger-copy)
(define-key dired-mode-map (kbd "X") 'dired-ranger-move)
(define-key dired-mode-map (kbd "Y") 'dired-ranger-paste)

(add-hook 'dired-mode-hook (lambda () (dired-collapse-mode)))
