(global-set-key (kbd "C-c c") 'compile)

(defun makefile-compile ()
  (let* ((root (eproject-root))
         (makefile (concat (file-name-as-directory root) "Makefile")))
    (when (file-readable-p makefile)
      (set (make-local-variable 'compile-command)
           (format "make -k -C '%s'" root))
      (set (make-local-variable 'compilation-read-command) nil))))

(add-hook 'generic-git-project-file-visit-hook 'makefile-compile)
