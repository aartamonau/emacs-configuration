(require 'yasnippet) ;; not yasnippet-bundle

(yas-global-mode 1)

(setq yas/root-directory "~/emacs/snippets")
(setq yas/prompt-functions '(yas/ido-prompt))

(yas/load-directory yas/root-directory)

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)
            (define-key yas/keymap [S-iso-lefttab] 'yas/prev-field)
            (define-key yas/keymap [(shift tab)] 'yas/prev-field)
            (define-key yas/keymap [backtab] 'yas/prev-field)))
