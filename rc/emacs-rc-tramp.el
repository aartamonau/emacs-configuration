(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(eval-after-load "tramp"
  '(progn
     (defun sudo-file-name (filename)
       (let ((sudo-tramp-prefix "/sudo:"))
         (set 'splitname (split-string filename ":"))
         (if (> (length splitname) 1)
             (progn (set 'final-split (cdr splitname))
                    (set 'sudo-tramp-prefix "/sudo:"))
           (progn (set 'final-split splitname)
                  (set 'sudo-tramp-prefix (concat sudo-tramp-prefix "root@localhost:"))))
         (set 'final-fn (concat sudo-tramp-prefix (mapconcat (lambda (e) e) final-split ":")))
         (message "splitname is %s" splitname)
         (message "sudo-tramp-prefix is %s" sudo-tramp-prefix)
         (message "final-split is %s" final-split)
         (message "final-fn is %s" final-fn)
         (message "%s" final-fn)))

     (defun sudo-find-file (filename &optional wildcards)
       "Calls find-file with filename with sudo-tramp-prefix prepended"
       (interactive "fFind file with sudo ")
       (let ((sudo-name (sudo-file-name filename)))
         (apply 'find-file
                (cons sudo-name (if (boundp 'wildcards) '(wildcards))))))

     (defun sudo-reopen-file ()
       "Reopen file as root by prefixing its name with sudo-tramp-prefix and by clearing buffer-read-only"
       (interactive)
       (let ((file-name (expand-file-name buffer-file-name))
             (buffer (current-buffer)))
         (sudo-find-file file-name)
         (kill-buffer buffer)))

     (global-set-key (kbd "C-c o") 'sudo-find-file)
     (global-set-key (kbd "C-c s") 'sudo-reopen-file)))
