;; emacs-rc-python.el --
(add-extension-exec-path "python")

(eval-after-load "flymake"
  '(progn
     (defun flymake-pylint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
			  'flymake-create-temp-inplace))
	      (local-file (file-relative-name
			   temp-file
			   (file-name-directory buffer-file-name))))
	 (list "epylint" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks
		  '("\\.py\\'" flymake-pylint-init))))

(add-hook
 'python-mode-hook
 '(lambda ()
    (define-key py-mode-map "\C-x?"
      'credmp/flymake-display-err-minibuf)))

(add-hook
 'python-mode-hook
 '(lambda ()
    (flymake-mode)))

;; (add-hook
;;  'python-mode-hook
;;  '(lambda ()
;;     (modify-syntax-entry ?\_ "."  py-mode-syntax-table)))
