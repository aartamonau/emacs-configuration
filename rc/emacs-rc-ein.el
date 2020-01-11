(require 'ein)
(require 'ein-notebook)
(require 'ein-subpackages)

(defun jupyter-start (directory)
  (interactive "D")
  (ein:jupyter-server-start "/usr/bin/jupyter" directory))

(defalias 'jupyter-stop 'ein:jupyter-server-stop)
