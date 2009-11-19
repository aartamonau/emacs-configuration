(let ((python-path (getenv "PYTHONPATH"))
      (ropemacs-path "~/emacs/ropemacs"))
  (unless (string-match ropemacs-path python-path)
    (setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":" ropemacs-path))))

(defun load-ropemacs ()
    "Load pymacs and ropemacs"
    (interactive)
    (require 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    ;; Automatically save project python buffers before refactorings
    (setq ropemacs-confirm-saving 'nil)
  )
(global-set-key "\C-xpl" 'load-ropemacs)
