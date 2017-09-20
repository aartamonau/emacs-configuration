(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-8"))

(setq frame-background-mode 'light)
(load-theme 'solarized)

(custom-set-faces
 '(diff-added ((t (:foreground "green3" :weight bold))))
 '(diff-removed ((t (:foreground "red3" :weight bold))))
 '(magit-item-highlight ((t (:background "#505050" :inherit nil)))))
