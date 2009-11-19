(defvar extensions-dir "~/emacs/extensions")

(setq load-path
      (append load-path
	      `(,extensions-dir "~/emacs/rc")))

(defun  add-extension-load-path (name)
  (setq load-path
	(append load-path
		(list (concat extensions-dir "/" name)))))
(defun add-extension-exec-path (name)
  (setq exec-path
	(append exec-path
		(list (concat extensions-dir "/" name)))))
