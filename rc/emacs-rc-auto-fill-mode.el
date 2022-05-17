;; fill only comments in modes that support those
(custom-set-variables
 '(comment-auto-fill-only-comments t)
 '(fill-column 78))

(add-hook 'text-mode-hook 'auto-fill-mode)
