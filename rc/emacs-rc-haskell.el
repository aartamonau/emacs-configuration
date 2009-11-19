(add-extension-exec-path "haskell")

(setq auto-mode-alist
      (append auto-mode-alist
	      '(("\\.[hg]s$"  . haskell-mode)
		("\\.hi$"     . haskell-mode)
		("\\.l[hg]s$" . literate-haskell-mode))))

(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)

(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)

(add-hook 'haskell-mode-hook 'global-hook-handler)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
(add-hook 'haskell-mode-hook #'(lambda () (require 'inf-haskell)))

(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))
(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "flycheck_haskell.pl"
	(list source base-dir)))

(eval-after-load "flymake"
  '(progn
     (push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
	   flymake-allowed-file-name-masks)
     (push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
	   flymake-allowed-file-name-masks)
     (push
      '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
	1 2 3 4) flymake-err-line-patterns)))

;; optional setting
;; if you want to use flymake always, then add the following hook.
(add-hook
 'haskell-mode-hook
 '(lambda ()
    (if (not (null buffer-file-name)) (flymake-mode))))

(add-hook
 'haskell-mode-hook
 '(lambda ()
    (define-key haskell-mode-map "\C-x?"
      'credmp/flymake-display-err-minibuf)))

;; (load "emacs-rc-pretty-lambda.el")

;; (defun haskell-unicode ()
;;   (interactive)
;;   (substitute-patterns-with-unicode
;;    (list (cons "\\(<-\\)" 'left-arrow)
;; 	 (cons "\\(->\\)" 'right-arrow)
;; 	 (cons "\\(==\\)" 'identical)
;; 	 (cons "\\(/=\\)" 'not-identical)
;; 	 (cons "\\(()\\)" 'nil)
;; 	 (cons "\\<\\(sqrt\\)\\>" 'square-root)
;; 	 (cons "\\(&&\\)" 'logical-and)
;; 	 (cons "\\(||\\)" 'logical-or)
;; 	 (cons "\\<\\(not\\)\\>" 'logical-neg)
;; 	 (cons "\\(>\\)\\[^=\\]" 'greater-than)
;; 	 (cons "\\(<\\)\\[^=\\]" 'less-than)
;; 	 (cons "\\(>=\\)" 'greater-than-or-equal-to)
;; 	 (cons "\\(<=\\)" 'less-than-or-equal-to)
;; 	 (cons "\\<\\(alpha\\)\\>" 'alpha)
;; 	 (cons "\\<\\(beta\\)\\>" 'beta)
;; 	 (cons "\\<\\(gamma\\)\\>" 'gamma)
;; 	 (cons "\\<\\(delta\\)\\>" 'delta)
;; 	 (cons "\\(''\\)" 'double-prime)
;; 	 (cons "\\('\\)" 'prime)
;; 	 (cons "\\(!!\\)" 'double-exclamation)
;; 	 (cons "\\(\\.\\.\\)" 'horizontal-ellipsis))))
;; (add-hook 'haskell-mode-hook 'haskell-unicode)
