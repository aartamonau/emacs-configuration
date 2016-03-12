(require 'haskell-mode-autoloads)

(add-hook 'haskell-mode-hook 'global-hook-handler)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (ghc-init)
            (setq ghc-display-error 'minibuffer)
            (setq ghc-ghc-options '("-Wall" "-fno-warn-name-shadowing"))))
(add-hook 'haskell-mode-hook 'turn-on-hi2)

(setq haskell-process-type 'auto)

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (let ((buffer
           (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                              (haskell-session-current-dir (haskell-session))
                              sym))))
      (with-current-buffer buffer
        (rename-buffer "*who-calls*")
        (switch-to-buffer-other-window buffer)))))

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
  '(progn (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
          (define-key haskell-mode-map [?\C-c ?\C-r] 'haskell-process-reload-file)
          (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
          ;; Build the Cabal project.
          (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal-build)
          ;; Get the type and info of the symbol at point, print it in the
          ;; message buffer.
          (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
          (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
          ;; Jump to the definition of the current symbol.
          (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
          (define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
          ;; Move the code below the current nesting left one.
          (define-key haskell-mode-map (kbd "C->") 'haskell-move-left)
          ;; Move the code below the current nesting right one.
          (define-key haskell-mode-map (kbd "C-<") 'haskell-move-right)
          (define-key haskell-mode-map (kbd "C-c C-s") 'toggle-scc-at-point)
          (define-key haskell-mode-map (kbd "C-c l") 'hs-lint)

          (define-key haskell-mode-map (kbd "C-c n") 'ghc-goto-next-error)
          (define-key haskell-mode-map (kbd "C-c p") 'ghc-goto-prev-error)
          (define-key haskell-mode-map (kbd "C-c ?") 'ghc-display-errors)))

(eval-after-load "haskell-cabal"
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal-build)))

(eval-after-load "haskell-interactive-mode"
  '(progn
     ;; Don't use C-c c or C-c C-c so that computations in ghci can still be killed.
     (define-key haskell-interactive-mode-map (kbd "C-c c") 'haskell-process-cabal-build)))

(custom-set-variables
 '(haskell-doc-show-reserved nil)
 '(haskell-doc-show-prelude nil)
 '(hindent-style "chris-done"))
