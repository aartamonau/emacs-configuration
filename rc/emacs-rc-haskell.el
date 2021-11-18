(require 'haskell-mode-autoloads)

(add-hook 'haskell-mode-hook 'global-hook-handler)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'lsp-deferred)

(setq haskell-process-type 'auto)
(setq haskell-compile-ignore-cabal t)

;; Based upon http://www.serpentine.com/blog/2007/10/09/using-emacs-to-insert-scc-annotations-in-haskell-code/
(defun toggle-scc-at-point (&optional arg)
  "Insert or kill (with universal-argument) an SCC annotation at
point."
  (interactive "P")
  (if (equal arg nil)
      (insert-scc-at-point)
    (kill-scc-at-point)))

(defun insert-scc-at-point ()
  "Insert an SCC annotation at point."
  (interactive)
  (if (or (looking-at "\\b\\|[ \t]\\|$") (and (not (bolp))
                                              (save-excursion
                                                (forward-char -1)
                                                (looking-at "\\b\\|[ \t]"))))
      (let ((space-at-point (looking-at "[ \t]")))
        (unless (and (not (bolp)) (save-excursion
                                    (forward-char -1)
                                    (looking-at "[ \t]")))
          (insert " "))
        (insert "{-# SCC \"\" #-}")
        (unless space-at-point
          (insert " "))
        (forward-char (if space-at-point -5 -6)))
    (error "Not over an area of whitespace")))

(defun kill-scc-at-point ()
  "Kill the SCC annotation at point."
  (interactive)
  (save-excursion
    (let ((old-point (point))
          (scc "\\({-#[ \t]*SCC \"[^\"]*\"[ \t]*#-}\\)[ \t]*"))
      (while (not (or (looking-at scc) (bolp)))
        (forward-char -1))
      (if (and (looking-at scc)
               (<= (match-beginning 1) old-point)
               (> (match-end 1) old-point))
          (kill-region (match-beginning 0) (match-end 0))
        (error "No SCC at point")))))

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
          (define-key haskell-mode-map (kbd "C-c C-s") 'toggle-scc-at-point)
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
