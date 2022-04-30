(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9.5"))
(custom-set-faces
 '(variable-pitch ((t (:family "DejaVu Sans Mono")))))

(load-theme 'doom-gruvbox)

(let ((face '((t (:bold t :foreground "#FC5C94" :background "#DDE6A7")))))
  (custom-set-faces
   `(whitespace-tab      ,face)
   `(whitespace-line     ,face)
   `(whitespace-trailing ,face)
   `(whitespace-empty    ,face)))
