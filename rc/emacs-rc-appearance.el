(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-8.5"))
(custom-set-faces
 '(variable-pitch ((t (:family "DejaVu Sans Mono")))))

(setq frame-background-mode 'light)
(load-theme 'solarized)

(let ((face '((t (:bold t :foreground "#FC5C94" :background "#DDE6A7")))))
  (custom-set-faces
   `(whitespace-tab      ,face)
   `(whitespace-line     ,face)
   `(whitespace-trailing ,face)
   `(whitespace-empty    ,face)))
