(defun aa/which-func-header-line-toggle ()
  (interactive)
  (if which-func-mode
      (progn (setq which-func-header-line-format
                   '(which-func-mode ("" which-func-format)))
             (setq mode-line-misc-info
                   (delete (assoc 'which-func-mode
                                  mode-line-misc-info) mode-line-misc-info))
             (setq header-line-format (list which-func-header-line-format)))
    (setq header-line-format nil)))

(defadvice which-func-ff-hook (after header-line activate)
  (aa/which-func-header-line-toggle))

(defadvice which-func-mode (after which-func-mode activate)
  (aa/which-func-header-line-toggle))
