(autoload 'gtags-mode "gtags"
  "Toggle Gtags mode, a minor mode for browsing source code using GLOBAL.")

(eval-after-load "gtags"
  '(progn

     (defun my/prefix-count (prefix-arg)
       "Number of times universal-argument has been pressed."
       (let ((result 0)
             (prefix prefix-arg))
         (while (> prefix 1)
           (setq prefix (/ prefix 4))
           (setq result (1+ result)))
         result))

     (defun my/gtags-find-file-at-point ()
       "Finds a file at point using gtags. Based on ffap-include-start.el."
       (interactive)

       (require 'thingatpt)
       (when (save-restriction
               (narrow-to-region (line-beginning-position) (line-end-position))
               (or
                (thing-at-point-looking-at
                 "include[ \t]+\"\\([^ \t\r\n\"]+\\)\"")

                (thing-at-point-looking-at
                 "^include[ \t]+\\([^ \t\r\n]+\\)")

                (thing-at-point-looking-at
                 "#[ \t]*include[ \t]+[\"<]\\([^\">\r\n]+\\)\\([\">]\\|$\\)")))
         (let ((tagname (buffer-substring-no-properties (match-beginning 1)
                                                        (match-end 1))))
           (gtags-push-context)
           (gtags-goto-tag tagname "P")
           t)))

     (defun my/gtags-find-tag-from-here ()
       "This is a custom version of gtags-find-tag-from-here that indicates
in the result value whether the tag jump has been actually done."
       (interactive)
       (let (tagname flag)
         (setq tagname (gtags-current-token))
         (if (not tagname)
             nil
           (gtags-push-context)
           (gtags-goto-tag tagname "C")
           t)))

     (defun my/gtags-find-file-or-tag-at-point ()
       "Tries to find a file and point at first. If this fails then tries to
find a tag at point. If even this fails then usual the falls back to usual
gtags-find-file."
       (interactive)

       (or (my/gtags-find-file-at-point)
           (my/gtags-find-tag-from-here)
           (gtags-find-tag)))

     (defun my/gtags-find-tag-dispatch (prefix)
       "Dispatches execution to specific find-tag function depending on
prefix argument."
       (interactive "p")
       (let ((prefix (my/prefix-count prefix)))
         (cond ((= prefix 0) (my/gtags-find-file-or-tag-at-point))
               ((= prefix 1) (gtags-find-tag))
               (t            (gtags-find-file)))))

     (defun my/gtags-find-rtag-at-point ()
       "Tries to find a cross-reference for tag at point. If this fails
then falls back to usual gtags-rtag-at-point function."
       (interactive)

       (let ((token (gtags-current-token)))
         (if token
             (progn (gtags-push-context)
                    (gtags-goto-tag token "r"))
           (gtags-find-rtag))))

     (defun my/gtags-find-rtag-dispatch (prefix)
       "Dispatches execution to specific find-rtag function depending on
prefix argument."
       (interactive "p")
       (let ((prefix (my/prefix-count prefix)))
         (cond ((= prefix 0) (my/gtags-find-rtag-at-point))
               (t            (gtags-find-rtag)))))


     (define-key gtags-mode-map "\M-." 'my/gtags-find-tag-dispatch)
     (define-key gtags-mode-map "\M-," 'my/gtags-find-rtag-dispatch)))

(add-hook 'c-mode-common-hook 'gtags-mode)
