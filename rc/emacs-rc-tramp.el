(eval-after-load "tramp"
  '(progn
     (defun sudo-file-name (filename)
       (concat "/sudo::" filename))

     (defun sudo-find-file (filename &optional wildcards)
       "Calls find-file with filename with sudo-tramp-prefix prepended"
       (interactive "fFind file with sudo ")
       (let ((sudo-name (sudo-file-name filename)))
         (apply 'find-file
                (cons sudo-name (if (boundp 'wildcards) '(wildcards))))))

     (defun sudo-reopen-file ()
       "Reopen file as root by prefixing its name with
sudo-tramp-prefix and by clearing buffer-read-only"
       (interactive)
       (let ((file-name (expand-file-name (or buffer-file-name
                                              list-buffers-directory)))
             (buffer (current-buffer)))
         (sudo-find-file file-name)
         (kill-buffer buffer)))

     (global-set-key (kbd "C-c o") 'sudo-find-file)
     (global-set-key (kbd "C-c s") 'sudo-reopen-file)))
