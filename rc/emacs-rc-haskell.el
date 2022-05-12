(require 'haskell-mode-autoloads)

(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'lsp-deferred)

(setq haskell-process-type 'auto)
(setq haskell-compile-ignore-cabal t)
(setq haskell-process-load-or-reload-prompt t)

(eval-after-load "haskell-mode"
  '(progn (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
          (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload-file)
          (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
          (define-key haskell-mode-map (kbd "C-c c") 'haskell-compile)))

(custom-set-variables
 '(haskell-doc-show-reserved nil)
 '(haskell-doc-show-prelude nil))
