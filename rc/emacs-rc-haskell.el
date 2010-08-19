(load "haskell-site-file")

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
