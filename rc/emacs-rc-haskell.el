(require 'haskell-mode-autoloads)

(add-hook 'haskell-mode-hook 'global-hook-handler)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'lsp-deferred)

(setq haskell-process-type 'auto)
(setq haskell-compile-ignore-cabal t)

(eval-after-load "haskell-mode"
  '(progn (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
          (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload-file)
          (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
          ;; Build the Cabal project.
          (define-key haskell-mode-map (kbd "C-c c")
            (lambda ()
              (interactive)
              (compile "stack build --fast")))
          ;; Get the type and info of the symbol at point, print it in the
          ;; message buffer.
          (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
          (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
          (define-key haskell-mode-map (kbd "C-c l") 'hs-lint)))

(eval-after-load "haskell-cabal"
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal-build)))

(eval-after-load "haskell-interactive-mode"
  '(progn
     ;; Don't use C-c c or C-c C-c so that computations in ghci can still be killed.
     (define-key haskell-interactive-mode-map (kbd "C-c c") 'haskell-process-cabal-build)))

(custom-set-variables
 '(haskell-doc-show-reserved nil)
 '(haskell-doc-show-prelude nil))
