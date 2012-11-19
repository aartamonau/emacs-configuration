(autoload 'doxymacs-mode "doxymacs" nil t)
(autoload 'doxymacs-font-lock "doxymacs" nil t)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

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
  (my-build-tab-stop-list 2)
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)) ;; force only spaces for indentation

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; doxymacs hook handler MUST be added after the previous handler
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'c-mode-common-hook 'auto-fill-mode)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
