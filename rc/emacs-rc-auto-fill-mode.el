;; fill only comments in modes that support those
(custom-set-variables
 '(comment-auto-fill-only-comments t)
 '(fill-column 78))

(defun my/auto-fill-mode-comments-only-disable ()
  "Some modes that support comments (and hence are concerned by the behavior
that comment-auto-fill-only-comments variable triggers) still need standard
auto-fill behavior. For those this function can be called somewhere in the
mode's startup hook instead of usual auto-fill-mode."
  (interactive)
  (auto-fill-mode)
  (set-variable 'comment-auto-fill-only-comments nil t))

(add-hook 'text-mode-hook 'auto-fill-mode)
