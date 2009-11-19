;; emacs-rc-hideshow.el

(load-library "hideshow")

(add-hook 'c-mode-hook
	  '(lambda ()
	     (hs-minor-mode 1)
	     (defadvice goto-line (after expand-after-goto-line
					 activate compile)
	       "hideshow-expand affected block when using goto-line in a collapsed buffer"
	       (save-excursion
		 (hs-show-block)))))