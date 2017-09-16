(defun my-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
	(counter 1)
	(ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (set (make-local-variable 'tab-stop-list) (nreverse ls))))

(defun my-c-mode-common-hook ()
  (c-set-style "k&r")
  (my-build-tab-stop-list 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil) ;; force only spaces for indentation
  (c-set-offset 'inextern-lang 0))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'c-mode-common-hook 'auto-fill-mode)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
