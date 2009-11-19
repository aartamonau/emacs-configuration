;; Xrefactory configuration part ;;
;; some Xrefactory defaults can be set here
(defvar xref-current-project nil) ;; can be also "my_project_name"
(defvar xref-key-binding 'global) ;; can be also 'local or 'none
(setq load-path (cons "~/emacs/extensions/xref/emacs" load-path))
(setq exec-path (cons "~/emacs/extensions/xref" exec-path))
(load "xrefactory")
;; end of Xrefactory configuration part ;;
(message "xrefactory loaded")

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (define-key c-mode-map "\M-/"
	      'xref-completion)))
