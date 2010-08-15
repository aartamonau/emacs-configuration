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
(add-hook 'haskell-mode-hook #'(lambda () (require 'inf-haskell)))


(defconst HaRe-path
  (car (directory-files "/usr/share/" t "HaRe.*")))

(when HaRe-path
  (add-extension-load-path HaRe-path)
  (autoload 'haskell-refac-mode "haskell-refac"
    "Minor mode for refactoring Haskell programs" t)
  (add-hook 'haskell-mode-hook 'haskell-refac-mode))


;; (load "emacs-rc-pretty-lambda.el")

;; (defun haskell-unicode ()
;;   (interactive)
;;   (substitute-patterns-with-unicode
;;    (list (cons "\\(<-\\)" 'left-arrow)
;;       (cons "\\(->\\)" 'right-arrow)
;;       (cons "\\(==\\)" 'identical)
;;       (cons "\\(/=\\)" 'not-identical)
;;       (cons "\\(()\\)" 'nil)
;;       (cons "\\<\\(sqrt\\)\\>" 'square-root)
;;       (cons "\\(&&\\)" 'logical-and)
;;       (cons "\\(||\\)" 'logical-or)
;;       (cons "\\<\\(not\\)\\>" 'logical-neg)
;;       (cons "\\(>\\)\\[^=\\]" 'greater-than)
;;       (cons "\\(<\\)\\[^=\\]" 'less-than)
;;       (cons "\\(>=\\)" 'greater-than-or-equal-to)
;;       (cons "\\(<=\\)" 'less-than-or-equal-to)
;;       (cons "\\<\\(alpha\\)\\>" 'alpha)
;;       (cons "\\<\\(beta\\)\\>" 'beta)
;;       (cons "\\<\\(gamma\\)\\>" 'gamma)
;;       (cons "\\<\\(delta\\)\\>" 'delta)
;;       (cons "\\(''\\)" 'double-prime)
;;       (cons "\\('\\)" 'prime)
;;       (cons "\\(!!\\)" 'double-exclamation)
;;       (cons "\\(\\.\\.\\)" 'horizontal-ellipsis))))
;; (add-hook 'haskell-mode-hook 'haskell-unicode)
