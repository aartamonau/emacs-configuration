(defvar extensions-dir "~/emacs/extensions")

(setq load-path
      (append load-path
              `(,extensions-dir "~/emacs/rc")))


(defun extension-path (path)
  "If supplied path is absolute then it's returned without changes. It's treated
as a file in extension directory."
  (if (file-name-absolute-p path)
      path
    (concat extensions-dir "/" name)))


(defun  add-extension-load-path (name)
  "Adds extension to the load-path."
  (setq load-path
        (append (list (extension-path name))
                load-path)))


(defun add-extension-exec-path (name)
  "Adds extension to the exec-path."
  (setq exec-path
        (append (list (extension-path name))
                exec-path)))
