(require 'haskell-mode-autoloads)

(add-hook 'haskell-mode-hook 'global-hook-handler)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (ghc-init)
            (setq ghc-ghc-options '("-Wall" "-fno-warn-name-shadowing"))
            (flymake-mode)))
(add-hook 'haskell-mode-hook 'turn-on-hi2)

;; Based upon https://github.com/paul7/dev-conf/blob/master/.emacs-haskell
(defvar cabal-use-sandbox t)
(setq haskell-process-type 'cabal-repl)
(defun cabal-toggle-sandboxing-local ()
  (interactive)
  (set (make-local-variable 'cabal-use-sandbox) (not cabal-use-sandbox))
  (message (format "This buffer haskell-process-type is ``%s''"
                   (set (make-local-variable 'haskell-process-type)
                        (if cabal-use-sandbox
                            'cabal-repl
                          'ghci)))))

(defun cabal-toggle-sandboxing ()
  (interactive)
  (setq cabal-use-sandbox (not cabal-use-sandbox))
  (message (format "haskell-process-type is ``%s''"
                   (setq haskell-process-type
                        (if cabal-use-sandbox
                            'cabal-repl
                          'ghci)))))

(eval-after-load "haskell-mode"
  '(progn (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
          (define-key haskell-mode-map [?\C-c ?\C-r] 'haskell-process-reload-file)))
