(global-whitespace-mode 1)

(custom-set-variables
 '(whitespace-global-modes '(not org-mode
                                 dired-mode
                                 magit-status-mode
                                 magit-diff-mode
                                 magit-revision-mode
                                 magit-stash-mode))
 '(whitespace-style '(face tabs trailing lines-tail empty tab-mark))
 '(whitespace-line-column 100))
